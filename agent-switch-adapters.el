;;; agent-switch-adapters.el --- Built-in LLM client adapters -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Built-in Claude Code, Codex, and OpenCode Adapters.

;;; Code:

(require 'cl-lib)
(require 'diff)
(require 'subr-x)
(require 'url-parse)
(require 'agent-switch-core)
(require 'agent-switch-storage)

(declare-function tomelr-encode "tomelr")
(declare-function toml:read-from-string "toml")

(defcustom agent-switch-claude-config-directory
  (expand-file-name "~/.claude/")
  "Claude Code configuration directory."
  :type 'directory
  :group 'agent-switch)

(defcustom agent-switch-codex-home
  (expand-file-name "~/.codex/")
  "Codex home directory."
  :type 'directory
  :group 'agent-switch)

(defcustom agent-switch-opencode-config-file nil
  "OpenCode global configuration file.
When nil, prefer opencode.jsonc when it exists, otherwise use
opencode.json below XDG_CONFIG_HOME or ~/.config."
  :type '(choice (const :tag "Auto" nil) file)
  :group 'agent-switch)

(defcustom agent-switch-confirm-canonical-rewrite t
  "Whether to confirm the first canonical Codex TOML rewrite per source hash."
  :type 'boolean
  :group 'agent-switch)

(defconst agent-switch--claude-owned-env-keys
  '("ANTHROPIC_API_KEY"
    "ANTHROPIC_AUTH_TOKEN"
    "ANTHROPIC_BASE_URL"
    "ANTHROPIC_MODEL"
    "ANTHROPIC_DEFAULT_HAIKU_MODEL"
    "ANTHROPIC_DEFAULT_SONNET_MODEL"
    "ANTHROPIC_DEFAULT_OPUS_MODEL"
    "ANTHROPIC_SMALL_FAST_MODEL")
  "Claude environment keys owned by the built-in Adapter.")

(defun agent-switch--claude-settings-path ()
  "Return Claude Code global settings path."
  (expand-file-name "settings.json"
                    (file-name-as-directory
                     (expand-file-name agent-switch-claude-config-directory))))

(defun agent-switch--codex-config-path ()
  "Return Codex global configuration path."
  (expand-file-name "config.toml"
                    (file-name-as-directory
                     (expand-file-name agent-switch-codex-home))))

(defun agent-switch--opencode-default-directory ()
  "Return OpenCode global configuration directory."
  (expand-file-name
   "opencode/"
   (file-name-as-directory
    (or (getenv "XDG_CONFIG_HOME") (expand-file-name "~/.config/")))))

(defun agent-switch--opencode-config-path ()
  "Return effective OpenCode global configuration path."
  (if agent-switch-opencode-config-file
      (expand-file-name agent-switch-opencode-config-file)
    (let* ((directory (agent-switch--opencode-default-directory))
           (jsonc (expand-file-name "opencode.jsonc" directory)))
      (if (file-exists-p jsonc)
          jsonc
        (expand-file-name "opencode.json" directory)))))

(defun agent-switch--read-json-file (path)
  "Read JSON object from PATH, returning an empty object when absent."
  (if (file-exists-p path)
      (let ((value (agent-switch-parse-json
                    (agent-switch--read-file-text path)
                    (file-name-nondirectory path))))
        (unless (hash-table-p value)
          (signal 'agent-switch-validation-error
                  (list (format "%s must contain a JSON object"
                                (file-name-nondirectory path)))))
        value)
    (make-hash-table :test #'equal)))

(defun agent-switch--context-file-state (context path)
  "Return file state for PATH from activation CONTEXT."
  (let ((snapshot (plist-get context :snapshot)))
    (or (cl-find path snapshot :key #'agent-switch-file-state-path
                 :test #'equal)
        (agent-switch-capture-file path))))

(defun agent-switch--write-live-text (path text context)
  "Back up and atomically write TEXT to PATH using CONTEXT snapshot."
  (let ((state (agent-switch--context-file-state context path)))
    (agent-switch-backup-file path)
    (setf (agent-switch-file-state-hash state)
          (agent-switch-write-text-atomic
           path text (agent-switch-file-state-hash state) t))))

(defun agent-switch--write-live-json (path object context)
  "Back up and atomically write JSON OBJECT to PATH using CONTEXT."
  (agent-switch--write-live-text
   path (agent-switch-json-serialize object) context))

(defun agent-switch--rollback-files (_client snapshot _context)
  "Restore file SNAPSHOT for a built-in Adapter."
  (dolist (state snapshot)
    (agent-switch-restore-file state))
  t)

(defun agent-switch--secret-marker (value)
  "Return a non-reversible marker for secret VALUE."
  (let ((marker (make-hash-table :test #'equal)))
    (puthash "$secret_hash" (secure-hash 'sha256 value) marker)
    marker))

(defun agent-switch--secret-marker-p (value)
  "Return non-nil when VALUE is a secret marker."
  (and (hash-table-p value) (stringp (gethash "$secret_hash" value))))

(defun agent-switch--sensitive-key-p (key)
  "Return non-nil when KEY conventionally carries a secret."
  (and (stringp key)
       (string-match-p agent-switch--sensitive-key-regexp (downcase key))))

(defun agent-switch--redact-json-secrets (value &optional parent-key)
  "Copy JSON VALUE while replacing secrets below PARENT-KEY with hashes."
  (cond
   ((and parent-key
         (agent-switch--sensitive-key-p parent-key)
         (stringp value))
    (agent-switch--secret-marker value))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test #'equal)))
      (maphash (lambda (key child)
                 (puthash key (agent-switch--redact-json-secrets child key) copy))
               value)
      copy))
   ((vectorp value)
    (vconcat (mapcar (lambda (child)
                       (agent-switch--redact-json-secrets child parent-key))
                     (append value nil))))
   ((consp value)
    (mapcar (lambda (child)
              (agent-switch--redact-json-secrets child parent-key))
            value))
   (t value)))

(defun agent-switch--provider-base-url-key-p (key)
  "Return non-nil when KEY conventionally names a provider Base URL."
  (and (stringp key)
       (let ((normalized (downcase key)))
         (or (string-suffix-p "base_url" normalized)
             (string-suffix-p "baseurl" normalized)))))

(defun agent-switch--find-provider-base-url (value)
  "Return the first provider Base URL string found recursively in VALUE."
  (cond
   ((hash-table-p value)
    (let (found)
      (maphash
       (lambda (key child)
         (unless found
           (setq found
                 (if (and (agent-switch--provider-base-url-key-p key)
                          (stringp child)
                          (not (string-empty-p child)))
                     child
                   (agent-switch--find-provider-base-url child)))))
       value)
      found))
   ((vectorp value)
    (cl-loop for child across value
             thereis (agent-switch--find-provider-base-url child)))
   ((consp value)
    (cl-loop for child in value
             thereis (agent-switch--find-provider-base-url child)))
   (t nil)))

(defun agent-switch--capture-authinfo-machine (client current)
  "Return authinfo machine for CLIENT CURRENT provider state."
  (let* ((base-url (agent-switch--find-provider-base-url current))
         (machine (and base-url
                       (condition-case nil
                           (url-host (url-generic-parse-url base-url))
                         (error nil)))))
    (if (and (stringp machine) (not (string-empty-p machine)))
        machine
      (if (agent-switch-client-p client)
          (agent-switch-client-id client)
        "agent-switch"))))

(defun agent-switch--auth-source-reference (machine login &optional delivery)
  "Return an auth-source reference for MACHINE, LOGIN, and DELIVERY."
  (let ((reference (make-hash-table :test #'equal))
        (authinfo (make-hash-table :test #'equal)))
    (puthash "source" "auth-source" reference)
    (puthash "machine" machine authinfo)
    (puthash "login" login authinfo)
    (puthash "authinfo" authinfo reference)
    (when delivery (puthash "delivery" delivery reference))
    reference))

(defun agent-switch--captured-secret-reference (machine path)
  "Return an auth-source reference for authinfo MACHINE and secret PATH."
  (agent-switch--auth-source-reference
   machine (if path (string-join path ".") "secret")))

(defun agent-switch--capture-secret-safe-value (value machine &optional path)
  "Copy VALUE, replacing secret markers with authinfo MACHINE references."
  (cond
   ((agent-switch--secret-marker-p value)
    (agent-switch--captured-secret-reference machine path))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test #'equal)))
      (maphash
       (lambda (key child)
         (puthash
          key
          (agent-switch--capture-secret-safe-value
           child machine (append path (list (format "%s" key))))
          copy))
       value)
      copy))
   ((vectorp value)
    (let ((copy (make-vector (length value) nil)))
      (dotimes (index (length value))
        (aset copy index
              (agent-switch--capture-secret-safe-value
               (aref value index) machine
               (append path (list (number-to-string index))))))
      copy))
   ((consp value)
    (cl-loop for child in value
             for index from 0
             collect (agent-switch--capture-secret-safe-value
                      child machine
                      (append path (list (number-to-string index))))))
   (t value)))

(defun agent-switch--capture-current (client current _context)
  "Capture CURRENT with generated auth-source references for all secrets."
  (let ((machine (agent-switch--capture-authinfo-machine client current)))
    (agent-switch-capture-result-create
     :payload (agent-switch--capture-secret-safe-value current machine)
     :complete-p t
     :warnings nil)))

(defun agent-switch--json-subset-p (expected actual)
  "Return non-nil when JSON EXPECTED is represented by ACTUAL.
Secret references match any configured secret; resolved strings match hashed
secret markers exactly."
  (cond
   ((agent-switch-secret-reference-p expected)
    (or (agent-switch--secret-marker-p actual)
        (and (agent-switch-secret-reference-p actual)
             (agent-switch--json-value-equal-p expected actual))))
   ((agent-switch--secret-marker-p actual)
    (and (stringp expected)
         (equal (gethash "$secret_hash" actual)
                (secure-hash 'sha256 expected))))
   ((hash-table-p expected)
    (and (hash-table-p actual)
         (let ((matches t))
           (maphash (lambda (key value)
                      (let ((missing (make-symbol "missing")))
                        (let ((actual-value (gethash key actual missing)))
                          (unless (and (not (eq actual-value missing))
                                       (agent-switch--json-subset-p
                                        value actual-value))
                            (setq matches nil)))))
                    expected)
           matches)))
   ((vectorp expected)
    (and (vectorp actual)
         (= (length expected) (length actual))
         (cl-loop for index below (length expected)
                  always (agent-switch--json-subset-p
                          (aref expected index) (aref actual index)))))
   (t (equal expected actual))))

(defun agent-switch--json-object-exact-p (expected actual)
  "Return non-nil when JSON objects EXPECTED and ACTUAL match exactly.
Secret references in EXPECTED match redacted markers in ACTUAL."
  (and (hash-table-p expected)
       (hash-table-p actual)
       (= (hash-table-count expected) (hash-table-count actual))
       (agent-switch--json-subset-p expected actual)))

(defun agent-switch--optional-field-match-p (key expected actual)
  "Return non-nil when optional KEY has equal presence and value."
  (let ((missing (make-symbol "missing")))
    (let ((expected-value (gethash key expected missing))
          (actual-value (gethash key actual missing)))
      (and (eq (eq expected-value missing) (eq actual-value missing))
           (or (eq expected-value missing)
               (agent-switch--json-subset-p expected-value actual-value))))))

;;; Claude Code

(defun agent-switch--claude-owned-state (settings)
  "Extract secret-safe owned state from Claude SETTINGS.
Return nil when no ANTHROPIC_* keys are configured."
  (let ((owned-env (make-hash-table :test #'equal))
        (env (gethash "env" settings)))
    (when (hash-table-p env)
      (dolist (key agent-switch--claude-owned-env-keys)
        (let ((missing (make-symbol "missing")))
          (let ((value (gethash key env missing)))
            (unless (eq value missing)
              (puthash key
                       (if (agent-switch--sensitive-key-p key)
                           (if (stringp value)
                               (agent-switch--secret-marker value)
                             value)
                         value)
                       owned-env))))))
    (if (> (hash-table-count owned-env) 0)
        (let ((owned (make-hash-table :test #'equal)))
          (puthash "env" owned-env owned)
          owned)
      nil)))

(defun agent-switch--claude-current (_client _context)
  "Return current Claude provider-owned state."
  (agent-switch--claude-owned-state
   (agent-switch--read-json-file (agent-switch--claude-settings-path))))

(defun agent-switch--claude-validate (_client profile _context)
  "Validate Claude PROFILE."
  (let* ((payload (agent-switch-profile-payload profile))
         (env (gethash "env" payload)))
    (unless (hash-table-p env)
      (signal 'agent-switch-validation-error
              '("Claude payload requires an env object")))
    (maphash
     (lambda (key _value)
       (unless (member key agent-switch--claude-owned-env-keys)
         (signal 'agent-switch-validation-error
                 (list (format "Claude env key is not provider-owned: %s" key)))))
     env)
    t))

(defun agent-switch--claude-snapshot (_client _profile _context)
  "Snapshot Claude settings for rollback."
  (list (agent-switch-capture-file (agent-switch--claude-settings-path))))

(defun agent-switch--claude-activate (_client profile context)
  "Activate resolved Claude PROFILE using CONTEXT."
  (let* ((path (agent-switch--claude-settings-path))
         (settings (agent-switch--read-json-file path))
         (env (or (gethash "env" settings)
                  (let ((new (make-hash-table :test #'equal)))
                    (puthash "env" new settings)
                    new)))
         (profile-env (gethash "env" (agent-switch-profile-payload profile))))
    (unless (hash-table-p env)
      (setq env (make-hash-table :test #'equal))
      (puthash "env" env settings))
    (dolist (key agent-switch--claude-owned-env-keys)
      (remhash key env))
    (maphash (lambda (key value) (puthash key value env)) profile-env)
    (agent-switch--write-live-json path settings context)
    t))

(defun agent-switch--claude-profile-current-p (_client profile current _context)
  "Return non-nil when Claude PROFILE matches CURRENT state."
  (let ((expected-env (gethash "env" (agent-switch-profile-payload profile)))
        (actual-env (and (hash-table-p current) (gethash "env" current))))
    (agent-switch--json-object-exact-p expected-env actual-env)))

(defun agent-switch--claude-watch-paths (_client _context)
  "Return paths watched for Claude changes."
  (list (agent-switch--claude-settings-path)))

;;; TOML helpers and Codex

(defun agent-switch--ensure-toml ()
  "Load structural TOML dependencies or signal."
  (unless (and (require 'toml nil t) (require 'tomelr nil t))
    (signal 'agent-switch-error
            '("Codex support requires the toml and tomelr packages"))))

(defun agent-switch--alist-get (key alist &optional default)
  "Return string KEY from ALIST or DEFAULT."
  (let ((cell (assoc-string key alist t)))
    (if cell (cdr cell) default)))

(defun agent-switch--alist-set (key value alist)
  "Set string KEY to VALUE in ALIST and return ALIST."
  (let ((cell (assoc-string key alist t)))
    (if cell
        (setcdr cell value)
      (setq alist (cons (cons key value) alist)))
    alist))

(defun agent-switch--alist-delete (key alist)
  "Delete string KEY from ALIST."
  (cl-remove key alist :key #'car :test #'string-equal))

(defun agent-switch--toml-table-p (value)
  "Return non-nil when VALUE is a TOML table alist."
  (and (listp value)
       value
       (cl-every (lambda (entry)
                   (and (consp entry) (stringp (car entry))))
                 value)))

(defun agent-switch--toml-order (value)
  "Recursively order TOML VALUE with scalars before tables."
  (cond
   ((agent-switch--toml-table-p value)
    (let (scalars tables)
      (dolist (entry value)
        (let ((converted (cons (car entry)
                               (agent-switch--toml-order (cdr entry)))))
          (if (or (agent-switch--toml-table-p (cdr entry))
                  (and (vectorp (cdr entry))
                       (> (length (cdr entry)) 0)
                       (agent-switch--toml-table-p (aref (cdr entry) 0))))
              (push converted tables)
            (push converted scalars))))
      (append (nreverse scalars) (nreverse tables))))
   ((vectorp value)
    (vconcat (mapcar #'agent-switch--toml-order (append value nil))))
   (t value)))

(defun agent-switch--json-to-toml (value)
  "Convert JSON VALUE to tomelr-compatible data."
  (cond
   ((hash-table-p value)
    (let (alist)
      (maphash (lambda (key child)
                 (push (cons key (agent-switch--json-to-toml child)) alist))
               value)
      (nreverse alist)))
   ((vectorp value)
    (vconcat (mapcar #'agent-switch--json-to-toml (append value nil))))
   ((eq value agent-switch-json-false) :false)
   ((eq value agent-switch-json-null) nil)
   (t value)))

(defun agent-switch--toml-to-json (value)
  "Convert TOML VALUE to JSON-compatible data."
  (cond
   ((agent-switch--toml-table-p value)
    (let ((object (make-hash-table :test #'equal)))
      (dolist (entry value)
        (puthash (car entry) (agent-switch--toml-to-json (cdr entry)) object))
      object))
   ((vectorp value)
    (vconcat (mapcar #'agent-switch--toml-to-json (append value nil))))
   ((eq value :false) agent-switch-json-false)
   (t value)))

(defun agent-switch--read-toml-file (path)
  "Parse PATH as TOML, returning an empty alist when absent."
  (agent-switch--ensure-toml)
  (if (file-exists-p path)
      (condition-case nil
          (toml:read-from-string (agent-switch--read-file-text path))
        (error
         (signal 'agent-switch-validation-error
                 (list (format "Invalid TOML in %s"
                               (file-name-nondirectory path))))))
    nil))

(defun agent-switch--encode-toml (data)
  "Encode TOML DATA and verify that it reparses."
  (agent-switch--ensure-toml)
  (let* ((ordered (agent-switch--toml-order data))
         (text (tomelr-encode ordered)))
    (unless (string-suffix-p "\n" text)
      (setq text (concat text "\n")))
    (condition-case nil
        (toml:read-from-string text)
      (error
       (signal 'agent-switch-error
               '("Generated Codex TOML failed verification"))))
    text))

(defun agent-switch--codex-provider-state (config provider-id)
  "Return provider table for PROVIDER-ID from TOML CONFIG."
  (let ((providers (agent-switch--alist-get "model_providers" config)))
    (if (agent-switch--toml-table-p providers)
        (or (agent-switch--alist-get provider-id providers) nil)
      nil)))

(defconst agent-switch--codex-managed-openai-provider-id
  "agent-switch-openai"
  "Live Codex provider ID used for authinfo-managed OpenAI access.")

(defun agent-switch--codex-semantic-provider-id (live-provider-id)
  "Return Profile provider ID represented by LIVE-PROVIDER-ID."
  (if (equal live-provider-id agent-switch--codex-managed-openai-provider-id)
      "openai"
    live-provider-id))

(defun agent-switch--codex-live-provider-id (provider-id credential)
  "Return live provider ID for Profile PROVIDER-ID and CREDENTIAL."
  (if (and credential (equal provider-id "openai"))
      agent-switch--codex-managed-openai-provider-id
    provider-id))

(defun agent-switch--codex-openai-provider-defaults (provider)
  "Return PROVIDER with authinfo-managed OpenAI defaults."
  (let ((copy (agent-switch-json-copy provider)))
    (unless (gethash "name" copy)
      (puthash "name" "OpenAI (authinfo)" copy))
    (unless (gethash "base_url" copy)
      (puthash "base_url" "https://api.openai.com/v1" copy))
    (unless (gethash "wire_api" copy)
      (puthash "wire_api" "responses" copy))
    copy))

(defun agent-switch--codex-credential-reference (provider-id provider)
  "Return a command-delivered authinfo reference for PROVIDER-ID PROVIDER."
  (agent-switch--auth-source-reference
   (or (and (hash-table-p provider)
            (let ((base-url (agent-switch--find-provider-base-url provider)))
              (and base-url
                   (condition-case nil
                       (url-host (url-generic-parse-url base-url))
                     (error nil)))))
       provider-id)
   (format "codex.%s.api-key" provider-id)
   "command"))

(defun agent-switch--codex-auth-helper-command ()
  "Return the Emacs executable used for Codex command-backed auth."
  (expand-file-name invocation-name invocation-directory))

(defun agent-switch--codex-auth-helper-args (credential)
  "Return command arguments that resolve CREDENTIAL through authinfo."
  (let* ((authinfo (gethash "authinfo" credential))
         (machine (gethash "machine" authinfo))
         (login (gethash "login" authinfo))
         (library (or (locate-library "agent-switch-authinfo")
                      (signal 'agent-switch-error
                              '("agent-switch-authinfo library is unavailable"))))
         (directory (file-name-directory library)))
    (vconcat
     (list "-Q" "--batch" "-L" directory
           "-l" "agent-switch-authinfo"
           "--eval" "(agent-switch-authinfo-main)"
           "--" (expand-file-name agent-switch-authinfo-file)
           machine login))))

(defun agent-switch--codex-auth-table (credential)
  "Return a Codex command-backed auth table for CREDENTIAL."
  (let ((auth (make-hash-table :test #'equal)))
    (puthash "command" (agent-switch--codex-auth-helper-command) auth)
    (puthash "args" (agent-switch--codex-auth-helper-args credential) auth)
    (puthash "timeout_ms" 15000 auth)
    (puthash "refresh_interval_ms" 300000 auth)
    auth))

(defun agent-switch--codex-helper-auth-reference (auth)
  "Return the authinfo reference encoded by managed Codex AUTH, or nil."
  (when (hash-table-p auth)
    (let* ((args-value (gethash "args" auth))
           (args (cond ((vectorp args-value) (append args-value nil))
                       ((listp args-value) args-value)))
           (separator (and args (cl-position "--" args :test #'equal)))
           (tail (and separator (nthcdr (1+ separator) args))))
      (when (and (member "agent-switch-authinfo" args)
                 (member "(agent-switch-authinfo-main)" args)
                 (= (length tail) 3)
                 (equal (expand-file-name (nth 0 tail))
                        (expand-file-name agent-switch-authinfo-file)))
        (agent-switch--auth-source-reference
         (nth 1 tail) (nth 2 tail) "command")))))

(defun agent-switch--codex-normalize-provider-auth (provider-id provider)
  "Return (PROVIDER . CREDENTIAL) for live PROVIDER-ID PROVIDER state."
  (let* ((copy (agent-switch-json-copy provider))
         (credential
          (and (hash-table-p copy)
               (agent-switch--codex-helper-auth-reference
                (gethash "auth" copy)))))
    (when credential
      (remhash "auth" copy))
    (when (and (not credential) (hash-table-p copy) (gethash "env_key" copy))
      (setq credential
            (agent-switch--codex-credential-reference provider-id copy))
      (remhash "env_key" copy)
      (remhash "env_key_instructions" copy))
    (cons copy credential)))

(defun agent-switch--codex-current (_client _context)
  "Return current Codex provider-owned state.
Return nil when no provider is configured."
  (let* ((config (agent-switch--read-toml-file
                  (agent-switch--codex-config-path)))
         (live-provider-id (agent-switch--alist-get "model_provider" config))
         (provider-id (and live-provider-id
                           (agent-switch--codex-semantic-provider-id
                            live-provider-id))))
    (when live-provider-id
      (let ((payload (make-hash-table :test #'equal)))
        (puthash "provider-id" provider-id payload)
        (when-let* ((model (agent-switch--alist-get "model" config)))
          (puthash "model" model payload))
        (when-let* ((small (agent-switch--alist-get "small_model" config)))
          (puthash "small-model" small payload))
        (let ((provider-state
               (agent-switch--codex-provider-state config live-provider-id)))
          (pcase-let* ((provider
                        (if provider-state
                            (agent-switch--redact-json-secrets
                             (agent-switch--toml-to-json provider-state))
                          (make-hash-table :test #'equal)))
                       (provider
                        (if (equal provider-id "openai")
                            (agent-switch--codex-openai-provider-defaults
                             provider)
                          provider))
                       (`(,normalized . ,credential)
                        (agent-switch--codex-normalize-provider-auth
                         provider-id provider)))
            (unless (or credential
                        (member provider-id '("ollama" "lmstudio")))
              (setq credential
                    (agent-switch--codex-credential-reference
                     provider-id normalized)))
            (puthash "provider" normalized payload)
            (when credential
              (puthash "credential" credential payload))))
        payload))))

(defun agent-switch--codex-validate (_client profile _context)
  "Validate Codex PROFILE."
  (let* ((payload (agent-switch-profile-payload profile))
         (provider-id (gethash "provider-id" payload))
         (model (gethash "model" payload))
         (provider (gethash "provider" payload))
         (credential (gethash "credential" payload)))
    (unless (and (stringp provider-id) (not (string-empty-p provider-id)))
      (signal 'agent-switch-validation-error
              '("Codex provider-id is required")))
    (unless (and (stringp model) (not (string-empty-p model)))
      (signal 'agent-switch-validation-error '("Codex model is required")))
    (unless (or (null provider) (hash-table-p provider))
      (signal 'agent-switch-validation-error
              '("Codex provider patch must be an object")))
    (when (hash-table-p provider)
      (dolist (key '("env_key" "env_key_instructions" "env_http_headers"
                     "experimental_bearer_token" "requires_openai_auth"
                     "auth"))
        (when (gethash key provider)
          (signal 'agent-switch-validation-error
                  (list (format "Codex Profile cannot persist provider.%s; use credential authinfo"
                                key))))))
    (unless (or (member provider-id '("ollama" "lmstudio"))
                (and (agent-switch-secret-reference-p credential)
                     (equal (gethash "delivery" credential) "command")))
      (signal 'agent-switch-validation-error
              '("Codex credential must be a command-delivered authinfo reference")))
    t))

(defun agent-switch--codex-snapshot (_client _profile _context)
  "Snapshot Codex config for rollback."
  (list (agent-switch-capture-file (agent-switch--codex-config-path))))

(defun agent-switch--show-rewrite-diff (path old-text new-text)
  "Display a unified diff for PATH from OLD-TEXT to NEW-TEXT."
  (let ((old-file (make-temp-file "agent-switch-old-"))
        (new-file (make-temp-file "agent-switch-new-")))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert old-text))
          (with-temp-file new-file (insert new-text))
          (let ((buffer (diff-no-select old-file new-file "-u" t)))
            (with-current-buffer buffer
              (rename-buffer "*agent-switch canonical rewrite*" t))
            (display-buffer buffer)))
      (ignore-errors (delete-file old-file))
      (ignore-errors (delete-file new-file))))
  (message "Canonical rewrite preview for %s" (abbreviate-file-name path)))

(defun agent-switch--confirm-codex-rewrite (path old-text new-text context)
  "Confirm canonical rewrite of PATH from OLD-TEXT to NEW-TEXT.
CONTEXT determines whether an interactive confirmation is available."
  (when (and agent-switch-confirm-canonical-rewrite
             (file-exists-p path)
             (not (equal old-text new-text)))
    (let ((hash (agent-switch-content-hash old-text))
          (key "codex-config"))
      (unless (agent-switch-state-canonical-confirmed-p key hash)
        (unless (plist-get context :interactive)
          (signal 'agent-switch-error
                  '("Codex canonical rewrite requires interactive confirmation")))
        (agent-switch--show-rewrite-diff path old-text new-text)
        (unless (yes-or-no-p
                 "Rewrite Codex config.toml and lose comments/order? ")
          (user-error "Cancelled"))
        (agent-switch-state-confirm-canonical key hash)))))

(defun agent-switch--codex-activate (_client profile context)
  "Activate resolved Codex PROFILE using CONTEXT."
  (let* ((path (agent-switch--codex-config-path))
         (old-text (if (file-exists-p path)
                       (agent-switch--read-file-text path) ""))
         (config (agent-switch--read-toml-file path))
         (payload (agent-switch-profile-payload profile))
         (provider-id (gethash "provider-id" payload))
         (live-provider-id
          (agent-switch--codex-live-provider-id
           provider-id (gethash "credential" payload)))
         (model (gethash "model" payload))
         (small-model (gethash "small-model" payload))
         (credential (gethash "credential" payload))
         (patch (or (gethash "provider" payload)
                    (make-hash-table :test #'equal)))
         (providers (or (agent-switch--alist-get "model_providers" config) nil))
         (existing (or (and (agent-switch--toml-table-p providers)
                            (agent-switch--alist-get live-provider-id providers))
                       nil))
         (merged (agent-switch-json-deep-merge
                  (agent-switch--toml-to-json existing) patch)))
    (when credential
      (dolist (key '("env_key" "env_key_instructions" "env_http_headers"
                     "experimental_bearer_token" "requires_openai_auth"))
        (remhash key merged))
      (puthash "auth" (agent-switch--codex-auth-table credential) merged))
    (setq config (agent-switch--alist-set
                  "model_provider" live-provider-id config))
    (setq config (agent-switch--alist-set "model" model config))
    (setq config (if (and (stringp small-model)
                          (not (string-empty-p small-model)))
                     (agent-switch--alist-set "small_model" small-model config)
                   (agent-switch--alist-delete "small_model" config)))
    (unless (agent-switch--toml-table-p providers)
      (setq providers nil))
    (setq providers
          (agent-switch--alist-set live-provider-id
                                   (agent-switch--json-to-toml merged)
                                   providers))
    (setq config (agent-switch--alist-set "model_providers" providers config))
    (let ((new-text (agent-switch--encode-toml config)))
      (agent-switch--confirm-codex-rewrite
       path old-text new-text context)
      (agent-switch--write-live-text path new-text context))
    t))

(defun agent-switch--codex-profile-current-p (_client profile current _context)
  "Return non-nil when Codex PROFILE matches CURRENT."
  (let ((payload (agent-switch-profile-payload profile)))
    (and (hash-table-p current)
         (equal (gethash "provider-id" payload) (gethash "provider-id" current))
         (equal (gethash "model" payload) (gethash "model" current))
         (agent-switch--optional-field-match-p "small-model" payload current)
         (agent-switch--optional-field-match-p "credential" payload current)
         (agent-switch--json-subset-p
          (or (gethash "provider" payload) (make-hash-table :test #'equal))
          (or (gethash "provider" current) (make-hash-table :test #'equal))))))

(defun agent-switch--codex-watch-paths (_client _context)
  "Return paths watched for Codex changes."
  (list (agent-switch--codex-config-path)))

;;; OpenCode global JSON/JSONC

(defun agent-switch--jsonc-clean (text)
  "Return JSONC TEXT with comments and trailing commas removed.
String contents and line positions are preserved."
  (let ((length (length text))
        (index 0)
        (state 'normal)
        (output (get-buffer-create " *agent-switch-jsonc*")))
    (unwind-protect
        (with-current-buffer output
          (erase-buffer)
          (while (< index length)
            (let* ((char (aref text index))
                   (next (and (< (1+ index) length)
                              (aref text (1+ index)))))
              (pcase state
                ('string
                 (insert-char char)
                 (cond ((eq char ?\\) (setq state 'escape))
                       ((eq char ?\") (setq state 'normal))))
                ('escape
                 (insert-char char)
                 (setq state 'string))
                ('line-comment
                 (if (eq char ?\n)
                     (progn (insert-char char) (setq state 'normal))
                   (insert-char ?\s)))
                ('block-comment
                 (cond
                  ((and (eq char ?*) (eq next ?/))
                   (insert "  ")
                   (setq index (1+ index) state 'normal))
                  ((eq char ?\n) (insert-char char))
                  (t (insert-char ?\s))))
                (_
                 (cond
                  ((eq char ?\") (insert-char char) (setq state 'string))
                  ((and (eq char ?/) (eq next ?/))
                   (insert "  ")
                   (setq index (1+ index) state 'line-comment))
                  ((and (eq char ?/) (eq next ?*))
                   (insert "  ")
                   (setq index (1+ index) state 'block-comment))
                  (t (insert-char char))))))
            (setq index (1+ index)))
          (let ((without-comments (buffer-string)))
            (erase-buffer)
            (setq index 0 state 'normal length (length without-comments))
            (while (< index length)
              (let ((char (aref without-comments index)))
                (pcase state
                  ('string
                   (insert-char char)
                   (cond ((eq char ?\\) (setq state 'escape))
                         ((eq char ?\") (setq state 'normal))))
                  ('escape (insert-char char) (setq state 'string))
                  (_
                   (cond
                    ((eq char ?\") (insert-char char) (setq state 'string))
                    ((eq char ?,)
                     (let ((lookahead (1+ index)))
                       (while (and (< lookahead length)
                                   (memq (aref without-comments lookahead)
                                         '(?\s ?\t ?\r ?\n)))
                         (setq lookahead (1+ lookahead)))
                       (unless (and (< lookahead length)
                                    (memq (aref without-comments lookahead)
                                          '(?} ?\])))
                         (insert-char char))))
                    (t (insert-char char)))))
              (setq index (1+ index))))
            (buffer-string)))
      (kill-buffer output))))

(defun agent-switch--read-opencode-file (path)
  "Read OpenCode JSON or JSONC object from PATH."
  (if (not (file-exists-p path))
      (make-hash-table :test #'equal)
    (let ((value (agent-switch-parse-json
                  (agent-switch--jsonc-clean
                   (agent-switch--read-file-text path))
                  (file-name-nondirectory path))))
      (unless (hash-table-p value)
        (signal 'agent-switch-validation-error
                '("OpenCode global config must be a JSON object")))
      value)))

(defun agent-switch--model-provider-id (model)
  "Return provider prefix from OpenCode MODEL."
  (and (stringp model)
       (string-match "\\`\\([^/]+\\)/" model)
       (match-string 1 model)))

(defun agent-switch--opencode-current (_client _context)
  "Return current OpenCode global provider-owned state.
Return nil when no model is configured."
  (let* ((config (agent-switch--read-opencode-file
                  (agent-switch--opencode-config-path)))
         (model (gethash "model" config))
         (provider-id (agent-switch--model-provider-id model)))
    (when model
      (let ((payload (make-hash-table :test #'equal))
            (providers (gethash "provider" config)))
        (when provider-id (puthash "provider-id" provider-id payload))
        (puthash "model" model payload)
        (when-let* ((small (gethash "small_model" config)))
          (puthash "small-model" small payload))
        (when (and provider-id (hash-table-p providers))
          (puthash "provider"
                   (agent-switch--redact-json-secrets
                    (or (gethash provider-id providers)
                        (make-hash-table :test #'equal)))
                   payload))
        payload))))

(defun agent-switch--opencode-validate (_client profile _context)
  "Validate OpenCode PROFILE."
  (let* ((payload (agent-switch-profile-payload profile))
         (provider-id (gethash "provider-id" payload))
         (model (gethash "model" payload))
         (provider (gethash "provider" payload)))
    (unless (and (stringp provider-id) (not (string-empty-p provider-id)))
      (signal 'agent-switch-validation-error
              '("OpenCode provider-id is required")))
    (unless (and (stringp model)
                 (equal (agent-switch--model-provider-id model) provider-id))
      (signal 'agent-switch-validation-error
              '("OpenCode model must use provider-id/model-id form")))
    (unless (or (null provider) (hash-table-p provider))
      (signal 'agent-switch-validation-error
              '("OpenCode provider patch must be an object")))
    t))

(defun agent-switch--opencode-snapshot (_client _profile _context)
  "Snapshot OpenCode global config for rollback."
  (list (agent-switch-capture-file (agent-switch--opencode-config-path))))

(defun agent-switch--opencode-activate (_client profile context)
  "Activate resolved OpenCode PROFILE globally using CONTEXT."
  (let* ((path (agent-switch--opencode-config-path))
         (config (agent-switch--read-opencode-file path))
         (payload (agent-switch-profile-payload profile))
         (provider-id (gethash "provider-id" payload))
         (model (gethash "model" payload))
         (small-model (gethash "small-model" payload))
         (patch (or (gethash "provider" payload)
                    (make-hash-table :test #'equal)))
         (providers (or (gethash "provider" config)
                        (let ((new (make-hash-table :test #'equal)))
                          (puthash "provider" new config)
                          new)))
         (existing (or (gethash provider-id providers)
                       (make-hash-table :test #'equal))))
    (unless (hash-table-p providers)
      (setq providers (make-hash-table :test #'equal))
      (puthash "provider" providers config))
    (puthash provider-id (agent-switch-json-deep-merge existing patch) providers)
    (puthash "model" model config)
    (if (and (stringp small-model) (not (string-empty-p small-model)))
        (puthash "small_model" small-model config)
      (remhash "small_model" config))
    (agent-switch--write-live-json path config context)
    t))

(defun agent-switch--opencode-profile-current-p (_client profile current _context)
  "Return non-nil when OpenCode PROFILE matches CURRENT."
  (let ((payload (agent-switch-profile-payload profile)))
    (and (hash-table-p current)
         (equal (gethash "provider-id" payload) (gethash "provider-id" current))
         (equal (gethash "model" payload) (gethash "model" current))
         (agent-switch--optional-field-match-p "small-model" payload current)
         (agent-switch--json-subset-p
          (or (gethash "provider" payload) (make-hash-table :test #'equal))
          (or (gethash "provider" current) (make-hash-table :test #'equal))))))

(defun agent-switch--opencode-watch-paths (_client _context)
  "Return paths watched for OpenCode global changes."
  (list (agent-switch--opencode-config-path)))

;;; Registration

(defun agent-switch--template-object (&rest entries)
  "Return a JSON object populated from ENTRIES cons cells."
  (let ((object (make-hash-table :test #'equal)))
    (dolist (entry entries object)
      (puthash (car entry) (cdr entry) object))))

(defun agent-switch--claude-profile-columns (_client profile _context)
  "Return Claude model and provider Base URL columns for PROFILE."
  (let* ((env (gethash "env" (agent-switch-profile-payload profile)))
         (model (and (hash-table-p env)
                     (or (gethash "ANTHROPIC_MODEL" env)
                         (gethash "ANTHROPIC_DEFAULT_SONNET_MODEL" env)
                         (gethash "ANTHROPIC_DEFAULT_OPUS_MODEL" env)
                         (gethash "ANTHROPIC_DEFAULT_HAIKU_MODEL" env))))
         (base-url (and (hash-table-p env)
                        (gethash "ANTHROPIC_BASE_URL" env))))
    (list :model model :base-url base-url)))

(defun agent-switch--codex-profile-columns (_client profile _context)
  "Return Codex model and provider Base URL columns for PROFILE."
  (let* ((payload (agent-switch-profile-payload profile))
         (provider (gethash "provider" payload)))
    (list :model (gethash "model" payload)
          :base-url (and (hash-table-p provider)
                         (gethash "base_url" provider)))))

(defun agent-switch--opencode-profile-columns (_client profile _context)
  "Return OpenCode model and provider Base URL columns for PROFILE."
  (let* ((payload (agent-switch-profile-payload profile))
         (provider (gethash "provider" payload))
         (options (and (hash-table-p provider)
                       (gethash "options" provider))))
    (list :model (gethash "model" payload)
          :base-url (and (hash-table-p options)
                         (gethash "baseURL" options)))))

(defun agent-switch--claude-profile-template (_client _context)
  "Return a new Claude Profile payload template."
  (agent-switch--template-object
   (cons "env" (agent-switch--template-object
                '("ANTHROPIC_BASE_URL" . "")
                '("ANTHROPIC_MODEL" . "")
                '("ANTHROPIC_DEFAULT_HAIKU_MODEL" . "")
                '("ANTHROPIC_DEFAULT_SONNET_MODEL" . "")
                '("ANTHROPIC_DEFAULT_OPUS_MODEL" . "")))))

(defun agent-switch--codex-profile-template (_client _context)
  "Return a new Codex Profile payload template."
  (agent-switch--template-object
   '("provider-id" . "") '("model" . "") '("small-model" . "")
   (cons "provider" (agent-switch--template-object
                     '("base_url" . "")
                     '("wire_api" . "responses")))
   (cons "credential"
         (agent-switch--auth-source-reference "" "" "command"))))

(defun agent-switch--opencode-profile-template (_client _context)
  "Return a new OpenCode Profile payload template."
  (agent-switch--template-object
   '("provider-id" . "") '("model" . "") '("small-model" . "")
   (cons "provider" (agent-switch--template-object
                     '("npm" . "")
                     (cons "options" (agent-switch--template-object
                                      '("baseURL" . "")))))))

(defun agent-switch-register-builtins ()
  "Register built-in adapters and clients."
  (agent-switch-define-adapter claude
    :name "Claude Code"
    :current #'agent-switch--claude-current
    :activate #'agent-switch--claude-activate
    :validate #'agent-switch--claude-validate
    :snapshot #'agent-switch--claude-snapshot
    :rollback #'agent-switch--rollback-files
    :profile-current-p #'agent-switch--claude-profile-current-p
    :capture-current #'agent-switch--capture-current
    :watch-paths #'agent-switch--claude-watch-paths
    :profile-template #'agent-switch--claude-profile-template
    :profile-columns #'agent-switch--claude-profile-columns)
  (agent-switch-register-client 'claude :name "Claude Code" :adapter 'claude)

  (agent-switch-define-adapter codex
    :name "Codex"
    :payload-version 2
    :current #'agent-switch--codex-current
    :activate #'agent-switch--codex-activate
    :validate #'agent-switch--codex-validate
    :snapshot #'agent-switch--codex-snapshot
    :rollback #'agent-switch--rollback-files
    :profile-current-p #'agent-switch--codex-profile-current-p
    :capture-current #'agent-switch--capture-current
    :watch-paths #'agent-switch--codex-watch-paths
    :profile-template #'agent-switch--codex-profile-template
    :profile-columns #'agent-switch--codex-profile-columns)
  (agent-switch-register-client 'codex :name "Codex" :adapter 'codex)

  (agent-switch-define-adapter opencode
    :name "OpenCode"
    :current #'agent-switch--opencode-current
    :activate #'agent-switch--opencode-activate
    :validate #'agent-switch--opencode-validate
    :snapshot #'agent-switch--opencode-snapshot
    :rollback #'agent-switch--rollback-files
    :profile-current-p #'agent-switch--opencode-profile-current-p
    :capture-current #'agent-switch--capture-current
    :watch-paths #'agent-switch--opencode-watch-paths
    :profile-template #'agent-switch--opencode-profile-template
    :profile-columns #'agent-switch--opencode-profile-columns)
  (agent-switch-register-client 'opencode
                                :name "OpenCode"
                                :adapter 'opencode))

(agent-switch-register-builtins)

(provide 'agent-switch-adapters)

;;; agent-switch-adapters.el ends here
