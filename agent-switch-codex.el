;;; agent-switch-codex.el --- Codex adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Built-in Codex adapter.

;;; Code:

(require 'cl-lib)
(require 'diff)
(require 'subr-x)
(require 'agent-switch-adapter-utils)

(declare-function tomelr-encode "tomelr")
(declare-function toml:read-from-string "toml")

(defcustom agent-switch-codex-home
  (expand-file-name "~/.codex/")
  "Codex home directory."
  :type 'directory
  :group 'agent-switch)

(defcustom agent-switch-confirm-canonical-rewrite t
  "Whether to confirm the first canonical Codex TOML rewrite per source hash."
  :type 'boolean
  :group 'agent-switch)

(defun agent-switch--codex-config-path ()
  "Return Codex global configuration path."
  (expand-file-name "config.toml"
                    (file-name-as-directory
                     (expand-file-name agent-switch-codex-home))))

(defun agent-switch--codex-secret-location-comments ()
  "Return likely pre-adoption Codex secret locations."
  (list
   (format "An API key may currently come from the environment variable named by model_providers.<id>.env_key in %s."
           (abbreviate-file-name (agent-switch--codex-config-path)))
   (format "Native ChatGPT/OAuth credentials may be in %s; agent-switch leaves them unmanaged and they are not API keys to copy here."
           (abbreviate-file-name
            (expand-file-name "auth.json" agent-switch-codex-home)))))

(defun agent-switch--codex-capture-current (client current _context)
  "Capture CLIENT CURRENT state with Codex-specific secret hints."
  (agent-switch--capture-current-with-comments
   client current (agent-switch--codex-secret-location-comments)))

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
   (agent-switch--provider-authinfo-machine provider provider-id)
   (format "codex.%s.api-key" provider-id)
   "command"
   (agent-switch--codex-secret-location-comments)))

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
         (nth 1 tail) (nth 2 tail) "command"
         (agent-switch--codex-secret-location-comments))))))

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

(defun agent-switch--codex-provider-payload (config live-provider-id)
  "Return a Profile payload for LIVE-PROVIDER-ID in Codex CONFIG."
  (let* ((provider-id
          (agent-switch--codex-semantic-provider-id live-provider-id))
         (payload (make-hash-table :test #'equal))
         (provider-state
          (agent-switch--codex-provider-state config live-provider-id))
         (provider
          (if provider-state
              (agent-switch--redact-json-secrets
               (agent-switch--toml-to-json provider-state))
            (make-hash-table :test #'equal))))
    (puthash "provider-id" provider-id payload)
    (when-let* ((model (agent-switch--alist-get "model" config)))
      (puthash "model" model payload))
    (when-let* ((small (agent-switch--alist-get "small_model" config)))
      (puthash "small-model" small payload))
    (when (equal provider-id "openai")
      (setq provider (agent-switch--codex-openai-provider-defaults provider)))
    (pcase-let ((`(,normalized . ,credential)
                 (agent-switch--codex-normalize-provider-auth
                  provider-id provider)))
      (unless (or credential (member provider-id '("ollama" "lmstudio")))
        (setq credential
              (agent-switch--codex-credential-reference
               provider-id normalized)))
      (puthash "provider" normalized payload)
      (when credential
        (puthash "credential" credential payload)))
    payload))

(defun agent-switch--codex-current (_client _context)
  "Return current Codex provider-owned state.
Return nil when no provider is configured."
  (let* ((config (agent-switch--read-toml-file
                  (agent-switch--codex-config-path)))
         (live-provider-id (agent-switch--alist-get "model_provider" config)))
    (when live-provider-id
      (agent-switch--codex-provider-payload config live-provider-id))))

(defun agent-switch--codex-discover (client _context)
  "Return Profiles for every provider table in Codex's global config."
  (let* ((config (agent-switch--read-toml-file
                  (agent-switch--codex-config-path)))
         (providers (agent-switch--alist-get "model_providers" config))
         profiles)
    (dolist (entry providers (nreverse profiles))
      (let* ((live-provider-id (car entry))
             (provider-id
              (agent-switch--codex-semantic-provider-id live-provider-id))
             (provider (cdr entry))
             (name (or (and (agent-switch--toml-table-p provider)
                            (agent-switch--alist-get "name" provider))
                       provider-id)))
        (push
         (agent-switch--make-profile
          :id provider-id
          :client-id (agent-switch-client-id client)
          :name name
          :payload (agent-switch--codex-provider-payload
                    config live-provider-id)
          :ownership 'external
          :source (agent-switch--codex-config-path)
          :valid-p t
          :payload-version 2)
         profiles)))))

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

(defun agent-switch--codex-profile-columns (_client profile _context)
  "Return Codex model and provider Base URL columns for PROFILE."
  (let* ((payload (agent-switch-profile-payload profile))
         (provider (gethash "provider" payload)))
    (list :model (gethash "model" payload)
          :base-url (and (hash-table-p provider)
                         (gethash "base_url" provider)))))

(defun agent-switch--codex-profile-template (_client _context)
  "Return a new Codex Profile payload template."
  (agent-switch--template-object
   '("provider-id" . "") '("model" . "") '("small-model" . "")
   (cons "provider" (agent-switch--template-object
                     '("base_url" . "")
                     '("wire_api" . "responses")))
   (cons "credential"
         (agent-switch--auth-source-reference
          "" "" "command"
          (agent-switch--codex-secret-location-comments)))))

(defun agent-switch-register-codex ()
  "Register the built-in Codex adapter and client."
  (agent-switch-define-adapter codex
    :name "Codex"
    :payload-version 2
    :current #'agent-switch--codex-current
    :discover #'agent-switch--codex-discover
    :activate #'agent-switch--codex-activate
    :validate #'agent-switch--codex-validate
    :snapshot #'agent-switch--codex-snapshot
    :rollback #'agent-switch--rollback-files
    :profile-current-p #'agent-switch--codex-profile-current-p
    :capture-current #'agent-switch--codex-capture-current
    :watch-paths #'agent-switch--codex-watch-paths
    :profile-template #'agent-switch--codex-profile-template
    :profile-columns #'agent-switch--codex-profile-columns)
  (agent-switch-register-client 'codex :name "Codex" :adapter 'codex))

(provide 'agent-switch-codex)

;;; agent-switch-codex.el ends here
