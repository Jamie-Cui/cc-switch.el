;;; agent-switch-opencode.el --- OpenCode adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Built-in OpenCode adapter.

;;; Code:

(require 'subr-x)
(require 'agent-switch-adapter-utils)

(defcustom agent-switch-opencode-config-file nil
  "OpenCode global configuration file.
When nil, prefer opencode.jsonc when it exists, otherwise use
opencode.json below XDG_CONFIG_HOME or ~/.config."
  :type '(choice (const :tag "Auto" nil) file)
  :group 'agent-switch)

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

(defun agent-switch--opencode-secret-location-comments ()
  "Return likely pre-adoption OpenCode secret locations."
  (list
   "Password may currently be in ~/.local/share/opencode/auth.json after /connect or `opencode auth login`."
   (format "It may instead be provider.<id>.options.apiKey in %s, or a referenced environment/.env variable."
           (abbreviate-file-name (agent-switch--opencode-config-path)))))

(defun agent-switch--opencode-capture-current (client current _context)
  "Capture CLIENT CURRENT state with OpenCode-specific secret hints."
  (agent-switch--capture-current-with-comments
   client current (agent-switch--opencode-secret-location-comments)))

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

(defun agent-switch--opencode-profile-columns (_client profile _context)
  "Return OpenCode model and provider Base URL columns for PROFILE."
  (let* ((payload (agent-switch-profile-payload profile))
         (provider (gethash "provider" payload))
         (options (and (hash-table-p provider)
                       (gethash "options" provider))))
    (list :model (gethash "model" payload)
          :base-url (and (hash-table-p options)
                         (gethash "baseURL" options)))))

(defun agent-switch--opencode-profile-template (_client _context)
  "Return a new OpenCode Profile payload template."
  (agent-switch--template-object
   '("provider-id" . "") '("model" . "") '("small-model" . "")
   (cons "provider" (agent-switch--template-object
                     '("npm" . "")
                     (cons "options" (agent-switch--template-object
                                      '("baseURL" . "")))))))

(defun agent-switch-register-opencode ()
  "Register the built-in OpenCode adapter and client."
  (agent-switch-define-adapter opencode
    :name "OpenCode"
    :current #'agent-switch--opencode-current
    :activate #'agent-switch--opencode-activate
    :validate #'agent-switch--opencode-validate
    :snapshot #'agent-switch--opencode-snapshot
    :rollback #'agent-switch--rollback-files
    :profile-current-p #'agent-switch--opencode-profile-current-p
    :capture-current #'agent-switch--opencode-capture-current
    :watch-paths #'agent-switch--opencode-watch-paths
    :profile-template #'agent-switch--opencode-profile-template
    :profile-columns #'agent-switch--opencode-profile-columns)
  (agent-switch-register-client 'opencode
                                :name "OpenCode"
                                :adapter 'opencode))

(provide 'agent-switch-opencode)

;;; agent-switch-opencode.el ends here
