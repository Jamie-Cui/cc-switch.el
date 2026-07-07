;;; cc-switch.el --- Switch cc-switch providers from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Jamie
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1") (transient "0.4"))
;; Version: 0.1.0
;; URL: https://github.com/jamie/cc-switch.el

;; This file is not part of GNU Emacs.

;; cc-switch.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; cc-switch.el is a pure-Elisp companion for cc-switch-cli.  It reads the
;; existing cc-switch SQLite database and writes Claude/Codex live config
;; files directly.  It intentionally implements only the core switch path.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'sqlite)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)

(declare-function evil-make-overriding-map "evil-core")
(declare-function evil-normalize-keymaps "evil-core")

(defgroup cc-switch nil
  "Switch cc-switch providers from Emacs."
  :group 'tools
  :prefix "cc-switch-")

(defcustom cc-switch-config-dir nil
  "Directory containing cc-switch state.
When nil, use `CC_SWITCH_CONFIG_DIR' or ~/.cc-switch."
  :type '(choice (const :tag "Auto" nil) directory))

(defcustom cc-switch-claude-config-dir nil
  "Claude Code config directory.
When nil, use `CLAUDE_CONFIG_DIR' or ~/.claude."
  :type '(choice (const :tag "Auto" nil) directory))

(defcustom cc-switch-codex-home nil
  "Codex home directory.
When nil, use `CODEX_HOME' if it names an existing directory, otherwise
use ~/.codex."
  :type '(choice (const :tag "Auto" nil) directory))

(defcustom cc-switch-buffer-name "*cc-switch*"
  "Name of the cc-switch dashboard buffer."
  :type 'string)

(define-error 'cc-switch-error "cc-switch error")

(cl-defstruct cc-switch--provider
  id app name settings-config website-url category created-at sort-index
  notes icon icon-color meta in-failover-queue current)

(defconst cc-switch--supported-apps '("claude" "codex"))
(defconst cc-switch--json-null :null)
(defconst cc-switch--json-false :false)

(defun cc-switch--env-nonempty (name)
  "Return environment variable NAME unless it is empty."
  (let ((value (getenv name)))
    (and value (not (string-empty-p (string-trim value))) value)))

(defun cc-switch--expand-dir (dir)
  "Expand DIR as a directory name."
  (file-name-as-directory (expand-file-name dir)))

(defun cc-switch--config-dir ()
  "Return effective cc-switch config directory."
  (cc-switch--expand-dir
   (or cc-switch-config-dir
       (cc-switch--env-nonempty "CC_SWITCH_CONFIG_DIR")
       "~/.cc-switch")))

(defun cc-switch--db-path ()
  "Return effective cc-switch SQLite database path."
  (expand-file-name "cc-switch.db" (cc-switch--config-dir)))

(defun cc-switch--legacy-config-path ()
  "Return legacy cc-switch JSON config path."
  (expand-file-name "config.json" (cc-switch--config-dir)))

(defun cc-switch--claude-config-dir ()
  "Return effective Claude Code config directory."
  (cc-switch--expand-dir
   (or cc-switch-claude-config-dir
       (cc-switch--env-nonempty "CLAUDE_CONFIG_DIR")
       "~/.claude")))

(defun cc-switch--codex-home ()
  "Return effective Codex home directory."
  (cc-switch--expand-dir
   (or cc-switch-codex-home
       (let ((codex-home (cc-switch--env-nonempty "CODEX_HOME")))
         (and codex-home (file-directory-p codex-home) codex-home))
       "~/.codex")))

(defun cc-switch--claude-settings-path ()
  "Return Claude Code live settings path."
  (expand-file-name "settings.json" (cc-switch--claude-config-dir)))

(defun cc-switch--codex-config-path ()
  "Return Codex live config path."
  (expand-file-name "config.toml" (cc-switch--codex-home)))

(defun cc-switch--codex-auth-path ()
  "Return Codex live auth path."
  (expand-file-name "auth.json" (cc-switch--codex-home)))

(defun cc-switch--normalize-app (app)
  "Return normalized APP string or signal an error."
  (let ((value (cond
                ((null app) nil)
                ((symbolp app) (symbol-name app))
                ((stringp app) app)
                (t (format "%s" app)))))
    (unless value
      (signal 'cc-switch-error
              (list "App is required; expected claude or codex")))
    (setq value (downcase (string-trim value)))
    (unless (member value cc-switch--supported-apps)
      (signal 'cc-switch-error
              (list (format "Unsupported app %S; expected claude or codex" app))))
    value))

(defun cc-switch--read-app ()
  "Read app explicitly."
  (cc-switch--normalize-app
   (completing-read "App: " cc-switch--supported-apps nil t)))

(defun cc-switch--parse-json (text &optional context)
  "Parse JSON TEXT into hash-table based values.
CONTEXT is used only in sanitized error messages."
  (condition-case nil
      (json-parse-string text
                         :object-type 'hash-table
                         :array-type 'array
                         :null-object cc-switch--json-null
                         :false-object cc-switch--json-false)
    (json-parse-error
     (signal 'cc-switch-error
             (list (format "Invalid JSON in %s"
                           (or context "cc-switch data")))))))

(defun cc-switch--json-serialize (value)
  "Serialize VALUE as JSON."
  (json-serialize value
                  :null-object cc-switch--json-null
                  :false-object cc-switch--json-false))

(defun cc-switch--json-copy (value)
  "Deep copy JSON VALUE."
  (cond
   ((hash-table-p value)
    (let ((copy (make-hash-table :test 'equal)))
      (maphash (lambda (key child)
                 (puthash key (cc-switch--json-copy child) copy))
               value)
      copy))
   ((vectorp value)
    (apply #'vector (mapcar #'cc-switch--json-copy (append value nil))))
   ((consp value)
    (mapcar #'cc-switch--json-copy value))
   (t value)))

(defun cc-switch--json-deep-merge (target source)
  "Return TARGET deeply merged with SOURCE.
SOURCE wins on scalar conflicts."
  (if (and (hash-table-p target) (hash-table-p source))
      (let ((merged (cc-switch--json-copy target)))
        (maphash
         (lambda (key source-value)
           (puthash key
                    (cc-switch--json-deep-merge
                     (gethash key merged)
                     source-value)
                    merged))
         source)
        merged)
    (cc-switch--json-copy source)))

(defun cc-switch--json-object-empty-p (value)
  "Return non-nil if VALUE is an empty JSON object."
  (and (hash-table-p value) (= (hash-table-count value) 0)))

(defun cc-switch--json-null-p (value)
  "Return non-nil if VALUE is JSON null."
  (eq value cc-switch--json-null))

(defun cc-switch--json-true-p (value)
  "Return non-nil if VALUE is JSON true."
  (eq value t))

(defun cc-switch--hash-get-any (table keys)
  "Return the first value in TABLE under one of KEYS.
Missing keys return nil."
  (when (hash-table-p table)
    (catch 'found
      (dolist (key keys)
        (when (gethash key table nil)
          (throw 'found (gethash key table))))
      nil)))

(defun cc-switch--provider-uses-common-config-p (provider snippet)
  "Return non-nil if PROVIDER opts into common config SNIPPET."
  (and snippet
       (not (string-empty-p (string-trim snippet)))
       (cc-switch--json-true-p
        (cc-switch--hash-get-any
         (cc-switch--provider-meta provider)
         '("commonConfigEnabled" "applyCommonConfig")))))

(defun cc-switch--sqlite-open (readonly)
  "Open the cc-switch database.
When READONLY is non-nil, open in read-only mode."
  (let ((path (cc-switch--db-path)))
    (unless (file-exists-p path)
      (if (file-exists-p (cc-switch--legacy-config-path))
          (signal 'cc-switch-error
                  (list "cc-switch.db does not exist; run cc-switch-cli once to migrate config.json"))
        (signal 'cc-switch-error
                (list (format "cc-switch.db does not exist: %s" path)))))
    (let ((db (cc-switch--sqlite-open-file path readonly)))
      (sqlite-execute db "PRAGMA busy_timeout = 1000")
      db)))

(defun cc-switch--sqlite-open-file (path &optional readonly)
  "Open SQLite database PATH.
READONLY is honored on Emacs builds whose `sqlite-open' supports it.
Emacs 29 accepts only PATH, so READONLY is ignored there."
  (condition-case nil
      (if readonly
          (apply #'sqlite-open (list path readonly))
        (sqlite-open path))
    (wrong-number-of-arguments
     (sqlite-open path))))

(defmacro cc-switch--with-db (binding &rest body)
  "Open a SQLite database for BODY.
BINDING is (VAR READONLY)."
  (declare (indent 1))
  (let ((var (car binding))
        (readonly (cadr binding)))
    `(let ((,var (cc-switch--sqlite-open ,readonly)))
       (unwind-protect
           (progn ,@body)
         (sqlite-close ,var)))))

(defun cc-switch--table-exists-p (db table)
  "Return non-nil if TABLE exists in DB."
  (not (null (sqlite-select
              db
              "SELECT name FROM sqlite_master WHERE type = 'table' AND name = ? LIMIT 1"
              (vector table)))))

(defun cc-switch--table-has-column-p (db table column)
  "Return non-nil if TABLE has COLUMN in DB."
  (and (cc-switch--table-exists-p db table)
       (cl-some (lambda (row) (equal (nth 1 row) column))
                (sqlite-select db (format "PRAGMA table_info(%s)" table)))))

(defun cc-switch--require-table (db table)
  "Signal if TABLE is missing in DB."
  (unless (cc-switch--table-exists-p db table)
    (signal 'cc-switch-error
            (list (format "cc-switch database is missing table %s" table)))))

(defun cc-switch--provider-from-row (row app)
  "Build a provider from SQLite ROW for APP."
  (let ((id (nth 0 row))
        (name (nth 1 row))
        (settings-config (nth 2 row))
        (website-url (nth 3 row))
        (category (nth 4 row))
        (created-at (nth 5 row))
        (sort-index (nth 6 row))
        (notes (nth 7 row))
        (icon (nth 8 row))
        (icon-color (nth 9 row))
        (meta (nth 10 row))
        (in-failover-queue (nth 11 row))
        (is-current (nth 12 row)))
    (make-cc-switch--provider
     :id id
     :app app
     :name name
     :settings-config (cc-switch--parse-json settings-config "provider settings")
     :website-url website-url
     :category category
     :created-at created-at
     :sort-index sort-index
     :notes notes
     :icon icon
     :icon-color icon-color
     :meta (cc-switch--parse-json (or meta "{}") "provider metadata")
     :in-failover-queue (not (zerop (or in-failover-queue 0)))
     :current (not (zerop (or is-current 0))))))

(defun cc-switch--providers (app)
  "Return providers for APP."
  (setq app (cc-switch--normalize-app app))
  (cc-switch--with-db (db t)
    (cc-switch--require-table db "providers")
    (mapcar
     (lambda (row) (cc-switch--provider-from-row row app))
     (sqlite-select
      db
      "SELECT id, name, settings_config, website_url, category, created_at, sort_index, notes, icon, icon_color, meta, in_failover_queue, is_current
       FROM providers
       WHERE app_type = ?
       ORDER BY COALESCE(sort_index, 999999), created_at ASC, id ASC"
      (vector app)))))

(defun cc-switch--current-provider-id (app)
  "Return current provider id for APP, or nil."
  (setq app (cc-switch--normalize-app app))
  (cc-switch--with-db (db t)
    (cc-switch--require-table db "providers")
    (caar (sqlite-select
           db
           "SELECT id FROM providers WHERE app_type = ? AND is_current = 1 LIMIT 1"
           (vector app)))))

(defun cc-switch--provider-by-id (app provider-id)
  "Return APP provider PROVIDER-ID or signal an error."
  (or (cl-find provider-id (cc-switch--providers app)
               :key #'cc-switch--provider-id
               :test #'equal)
      (signal 'cc-switch-error
              (list (format "Provider not found for %s: %s" app provider-id)))))

(defun cc-switch--setting (app suffix)
  "Return setting common to APP using key suffix SUFFIX."
  (cc-switch--with-db (db t)
    (when (cc-switch--table-exists-p db "settings")
      (caar (sqlite-select db
                           "SELECT value FROM settings WHERE key = ? LIMIT 1"
                           (vector (format "%s_%s" suffix app)))))))

(defun cc-switch--common-config-snippet (app)
  "Return common config snippet for APP, or nil."
  (cc-switch--setting app "common_config"))

(defun cc-switch--update-current-provider (app provider-id)
  "Set current provider for APP to PROVIDER-ID."
  (cc-switch--with-db (db nil)
    (cc-switch--require-table db "providers")
    (sqlite-execute db "BEGIN IMMEDIATE")
    (condition-case err
        (progn
          (sqlite-execute db
                          "UPDATE providers SET is_current = 0 WHERE app_type = ?"
                          (vector app))
          (sqlite-execute db
                          "UPDATE providers SET is_current = 1 WHERE app_type = ? AND id = ?"
                          (vector app provider-id))
          (sqlite-execute db "COMMIT"))
      (error
       (ignore-errors (sqlite-execute db "ROLLBACK"))
       (signal (car err) (cdr err))))))

(defun cc-switch--proxy-blocking-reason (app)
  "Return a reason string if APP appears proxy/takeover-managed."
  (cc-switch--with-db (db t)
    (cond
     ((and (cc-switch--table-exists-p db "proxy_live_backup")
           (sqlite-select db
                          "SELECT app_type FROM proxy_live_backup WHERE app_type = ? LIMIT 1"
                          (vector app)))
      "proxy live backup exists")
     ((and (cc-switch--table-has-column-p db "proxy_config" "enabled")
           (sqlite-select db
                          "SELECT app_type FROM proxy_config WHERE app_type = ? AND enabled = 1 LIMIT 1"
                          (vector app)))
      "proxy takeover is enabled")
     (t nil))))

(defun cc-switch--ensure-live-dir (app)
  "Signal if APP live config directory is not initialized."
  (let ((dir (pcase app
               ("claude" (cc-switch--claude-config-dir))
               ("codex" (cc-switch--codex-home)))))
    (unless (file-directory-p dir)
      (signal 'cc-switch-error
              (list (format "%s config directory does not exist: %s"
                            (capitalize app) dir))))))

(defun cc-switch--backup-path (path)
  "Return single backup path for PATH."
  (expand-file-name
   (concat "." (file-name-nondirectory path) ".cc-switch-el.bak")
   (file-name-directory path)))

(defun cc-switch--capture-file-state (path)
  "Capture PATH state for rollback."
  (list :path path
        :exists (file-exists-p path)
        :content (and (file-exists-p path)
                      (with-temp-buffer
                        (insert-file-contents-literally path)
                        (buffer-string)))))

(defun cc-switch--restore-file-state (state)
  "Restore file STATE captured by `cc-switch--capture-file-state'."
  (let ((path (plist-get state :path)))
    (if (plist-get state :exists)
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file path
            (insert (plist-get state :content))))
      (when (file-exists-p path)
        (delete-file path)))))

(defun cc-switch--restore-file-states (states)
  "Restore file STATES."
  (dolist (state states)
    (ignore-errors (cc-switch--restore-file-state state))))

(defun cc-switch--write-text-atomic (path text &optional create-parent)
  "Write TEXT to PATH atomically.
When CREATE-PARENT is non-nil, create the parent directory first."
  (let ((dir (file-name-directory path)))
    (when create-parent
      (make-directory dir t))
    (unless (file-directory-p dir)
      (signal 'cc-switch-error
              (list (format "Parent directory does not exist: %s" dir))))
    (let* ((base (file-name-nondirectory path))
           (temp (make-temp-file (expand-file-name (concat "." base ".tmp") dir))))
      (condition-case err
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file temp
              (insert text))
            (rename-file temp path t))
        (error
         (when (file-exists-p temp)
           (ignore-errors (delete-file temp)))
         (signal (car err) (cdr err)))))))

(defun cc-switch--copy-single-backup (path)
  "Copy PATH to its single cc-switch.el backup when it exists."
  (when (file-exists-p path)
    (copy-file path (cc-switch--backup-path path) t)))

(defun cc-switch--write-text-with-backup (path text &optional create-parent)
  "Write TEXT to PATH atomically after creating a single backup."
  (cc-switch--copy-single-backup path)
  (cc-switch--write-text-atomic path text create-parent))

(defun cc-switch--delete-file-with-backup (path)
  "Delete PATH after creating a single backup."
  (when (file-exists-p path)
    (cc-switch--copy-single-backup path)
    (delete-file path)))

(defun cc-switch--write-json-with-backup (path value &optional create-parent)
  "Write JSON VALUE to PATH after creating a single backup."
  (cc-switch--write-text-with-backup
   path
   (concat (cc-switch--json-serialize value) "\n")
   create-parent))

(defun cc-switch--effective-claude-settings (provider snippet)
  "Return effective Claude settings for PROVIDER and common SNIPPET."
  (let ((settings (cc-switch--json-copy (cc-switch--provider-settings-config provider))))
    (when (cc-switch--provider-uses-common-config-p provider snippet)
      (setq settings
            (cc-switch--json-deep-merge
             settings
             (cc-switch--parse-json snippet "Claude common config"))))
    (cc-switch--normalize-claude-models settings)
    (cc-switch--sanitize-claude-settings settings)
    settings))

(defun cc-switch--sanitize-claude-settings (settings)
  "Remove cc-switch internal-only keys from Claude SETTINGS."
  (when (hash-table-p settings)
    (dolist (key '("api_format" "apiFormat"
                   "openrouter_compat_mode" "openrouterCompatMode"))
      (remhash key settings)))
  settings)

(defun cc-switch--normalize-claude-models (settings)
  "Normalize legacy Claude model keys in SETTINGS."
  (let ((env (and (hash-table-p settings) (gethash "env" settings))))
    (when (hash-table-p env)
      (let* ((model (gethash "ANTHROPIC_MODEL" env))
             (small-fast (gethash "ANTHROPIC_SMALL_FAST_MODEL" env))
             (haiku (or (gethash "ANTHROPIC_DEFAULT_HAIKU_MODEL" env)
                        small-fast model))
             (sonnet (or (gethash "ANTHROPIC_DEFAULT_SONNET_MODEL" env)
                         model small-fast))
             (opus (or (gethash "ANTHROPIC_DEFAULT_OPUS_MODEL" env)
                       model small-fast)))
        (when (and haiku (not (gethash "ANTHROPIC_DEFAULT_HAIKU_MODEL" env)))
          (puthash "ANTHROPIC_DEFAULT_HAIKU_MODEL" haiku env))
        (when (and sonnet (not (gethash "ANTHROPIC_DEFAULT_SONNET_MODEL" env)))
          (puthash "ANTHROPIC_DEFAULT_SONNET_MODEL" sonnet env))
        (when (and opus (not (gethash "ANTHROPIC_DEFAULT_OPUS_MODEL" env)))
          (puthash "ANTHROPIC_DEFAULT_OPUS_MODEL" opus env))
        (remhash "ANTHROPIC_SMALL_FAST_MODEL" env))))
  settings)

(defun cc-switch--ensure-toml ()
  "Ensure TOML support is available for Codex."
  (unless (require 'toml nil t)
    (signal 'cc-switch-error
            (list "Codex support requires toml.el; install the toml package"))))

(defun cc-switch--merge-codex-common-config (config snippet)
  "Merge Codex TOML CONFIG with common config SNIPPET.
V1 requires toml.el for this path.  The actual merge is conservative:
the provider config is written as-is unless a non-empty common snippet is
present, in which case the snippet is prepended.  cc-switch-cli stores
provider snapshots stripped of common config, so this keeps the usual
path valid without attempting to preserve arbitrary duplicate TOML keys."
  (cc-switch--ensure-toml)
  (if (and snippet (not (string-empty-p (string-trim snippet))))
      (concat (string-trim-right snippet) "\n\n" (string-trim-left config))
    config))

(defun cc-switch--effective-codex-settings (provider snippet)
  "Return effective Codex settings for PROVIDER and common SNIPPET."
  (let* ((settings (cc-switch--provider-settings-config provider))
         (missing (make-symbol "missing"))
         (auth (gethash "auth" settings missing))
         (config (gethash "config" settings missing)))
    (when (eq auth missing)
      (signal 'cc-switch-error
              (list "Codex provider settings are missing the auth field")))
    (unless (stringp config)
      (signal 'cc-switch-error
              (list "Codex provider settings are missing string field config")))
    (let ((result (make-hash-table :test 'equal)))
      (puthash "auth" (cc-switch--json-copy auth) result)
      (puthash "config"
               (cc-switch--merge-codex-common-config
                config
                (and (cc-switch--provider-uses-common-config-p provider snippet)
                     snippet))
               result)
      result)))

(defun cc-switch--codex-official-provider-p (provider)
  "Return non-nil if PROVIDER is a Codex official provider."
  (or (and (cc-switch--provider-category provider)
           (string-equal (downcase (cc-switch--provider-category provider)) "official"))
      (cc-switch--json-true-p
       (cc-switch--hash-get-any
        (cc-switch--provider-meta provider)
        '("codexOfficial" "codex_official")))))

(defun cc-switch--live-paths-for-switch (app provider)
  "Return live paths touched when switching APP to PROVIDER."
  (pcase app
    ("claude" (list (cc-switch--claude-settings-path)))
    ("codex" (cons (cc-switch--codex-config-path)
                   (and (cc-switch--codex-official-provider-p provider)
                        (list (cc-switch--codex-auth-path)))))))

(defun cc-switch--apply-live-config (app provider)
  "Write APP live config for PROVIDER."
  (cc-switch--ensure-live-dir app)
  (let ((snippet (cc-switch--common-config-snippet app)))
    (pcase app
      ("claude"
       (cc-switch--write-json-with-backup
        (cc-switch--claude-settings-path)
        (cc-switch--effective-claude-settings provider snippet)))
      ("codex"
       (let* ((settings (cc-switch--effective-codex-settings provider snippet))
              (auth (gethash "auth" settings))
              (config (gethash "config" settings))
              (official (cc-switch--codex-official-provider-p provider)))
         (cc-switch--write-text-with-backup
          (cc-switch--codex-config-path)
          (concat config (unless (string-suffix-p "\n" config) "\n")))
         (when official
           (if (or (cc-switch--json-null-p auth)
                   (cc-switch--json-object-empty-p auth))
               (cc-switch--delete-file-with-backup (cc-switch--codex-auth-path))
             (cc-switch--write-json-with-backup (cc-switch--codex-auth-path) auth))))))))

(defun cc-switch--switch-provider (app provider-id)
  "Switch APP to PROVIDER-ID."
  (setq app (cc-switch--normalize-app app))
  (let ((reason (cc-switch--proxy-blocking-reason app)))
    (when reason
      (signal 'cc-switch-error
              (list (format "Refusing to switch %s while %s" app reason)))))
  (let* ((provider (cc-switch--provider-by-id app provider-id))
         (paths (cc-switch--live-paths-for-switch app provider))
         (states (mapcar #'cc-switch--capture-file-state paths)))
    (condition-case err
        (progn
          (cc-switch--apply-live-config app provider)
          (cc-switch--update-current-provider app provider-id)
          provider)
      (error
       (cc-switch--restore-file-states states)
       (signal (car err) (cdr err))))))

(defun cc-switch--provider-display (provider)
  "Return sanitized display string for PROVIDER."
  (format "%s %s [%s] %s"
          (if (cc-switch--provider-current provider) "*" " ")
          (cc-switch--provider-name provider)
          (cc-switch--provider-id provider)
          (or (cc-switch--provider-category provider) "-")))

(defun cc-switch--read-provider-id (app &optional prompt)
  "Read a provider id for APP using PROMPT."
  (let* ((providers (cc-switch--providers app))
         (candidates
          (mapcar (lambda (provider)
                    (cons (cc-switch--provider-display provider)
                          (cc-switch--provider-id provider)))
                  providers)))
    (unless candidates
      (signal 'cc-switch-error
              (list (format "No providers found for %s" app))))
    (cdr (assoc (completing-read (or prompt "Provider: ")
                                 candidates nil t)
                candidates))))

;;;###autoload
(defun cc-switch-provider-list (&optional app)
  "Show providers for APP.
Interactively, prompt for APP."
  (interactive (list (cc-switch--read-app)))
  (setq app (cc-switch--normalize-app app))
  (let ((providers (cc-switch--providers app)))
    (with-current-buffer (get-buffer-create "*cc-switch providers*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Providers for %s\n\n" app))
        (dolist (provider providers)
          (insert (cc-switch--provider-display provider) "\n"))
        (special-mode))
      (display-buffer (current-buffer)))
    providers))

;;;###autoload
(defun cc-switch-provider-current (&optional app)
  "Show current provider for APP.
Interactively, prompt for APP."
  (interactive (list (cc-switch--read-app)))
  (setq app (cc-switch--normalize-app app))
  (let ((provider-id (cc-switch--current-provider-id app)))
    (if (called-interactively-p 'interactive)
        (message "%s current provider: %s" app (or provider-id "<none>")))
    provider-id))

;;;###autoload
(defun cc-switch-provider-switch (&optional app provider-id)
  "Switch APP to PROVIDER-ID.
Interactively, prompt for APP and PROVIDER-ID."
  (interactive
   (let* ((app (cc-switch--read-app))
          (provider-id (cc-switch--read-provider-id app "Switch to provider: ")))
     (list app provider-id)))
  (let ((provider (cc-switch--switch-provider app provider-id)))
    (when (called-interactively-p 'interactive)
      (message "Switched %s to %s [%s]"
               (cc-switch--normalize-app app)
               (cc-switch--provider-name provider)
               (cc-switch--provider-id provider)))
    provider))

;;;###autoload
(defalias 'cc-switch-use #'cc-switch-provider-switch)

;;;###autoload
(defun cc-switch-switch-claude (&optional provider-id)
  "Switch Claude to PROVIDER-ID."
  (interactive (list (cc-switch--read-provider-id "claude" "Switch Claude to: ")))
  (cc-switch-provider-switch "claude" provider-id))

;;;###autoload
(defun cc-switch-switch-codex (&optional provider-id)
  "Switch Codex to PROVIDER-ID."
  (interactive (list (cc-switch--read-provider-id "codex" "Switch Codex to: ")))
  (cc-switch-provider-switch "codex" provider-id))

;;;###autoload
(defun cc-switch-provider-export (&optional provider-id output)
  "Export Claude PROVIDER-ID to OUTPUT.
Interactively, export to ./.claude/settings.local.json unless a prefix
argument is used, in which case read the output path."
  (interactive
   (let* ((provider-id (cc-switch--read-provider-id "claude" "Export Claude provider: "))
          (default (expand-file-name ".claude/settings.local.json"
                                     default-directory))
          (output (if current-prefix-arg
                      (read-file-name "Export to: "
                                      (file-name-directory default)
                                      default nil
                                      (file-name-nondirectory default))
                    default)))
     (list provider-id output)))
  (let* ((provider (cc-switch--provider-by-id "claude" provider-id))
         (settings (cc-switch--effective-claude-settings
                    provider
                    (cc-switch--common-config-snippet "claude"))))
    (cc-switch--write-json-with-backup output settings t)
    (when (called-interactively-p 'interactive)
      (message "Exported Claude provider %s to %s" provider-id output))
    output))

(defun cc-switch--diagnose-lines ()
  "Return sanitized diagnostic lines."
  (let ((lines nil)
        (db-path (cc-switch--db-path)))
    (push (format "cc-switch config dir: %s" (cc-switch--config-dir)) lines)
    (push (format "database: %s" db-path) lines)
    (push (format "database exists: %s" (if (file-exists-p db-path) "yes" "no")) lines)
    (push (format "legacy config.json exists: %s"
                  (if (file-exists-p (cc-switch--legacy-config-path)) "yes" "no"))
          lines)
    (when (file-exists-p db-path)
      (push (format "database readable: %s" (if (file-readable-p db-path) "yes" "no")) lines)
      (push (format "database writable: %s" (if (file-writable-p db-path) "yes" "no")) lines)
      (condition-case err
          (cc-switch--with-db (db t)
            (dolist (table '("providers" "settings" "proxy_live_backup" "proxy_config"))
              (push (format "table %s: %s"
                            table
                            (if (cc-switch--table-exists-p db table) "yes" "no"))
                    lines))
            (dolist (app cc-switch--supported-apps)
              (push (format "%s current provider: %s"
                            app
                            (or (cc-switch--current-provider-id app) "<none>"))
                    lines)
              (push (format "%s proxy block: %s"
                            app
                            (or (cc-switch--proxy-blocking-reason app) "no"))
                    lines)))
        (error
         (push (format "database check error: %s" (error-message-string err)) lines))))
    (push (format "Claude config dir initialized: %s (%s)"
                  (if (file-directory-p (cc-switch--claude-config-dir)) "yes" "no")
                  (cc-switch--claude-config-dir))
          lines)
    (push (format "Codex home initialized: %s (%s)"
                  (if (file-directory-p (cc-switch--codex-home)) "yes" "no")
                  (cc-switch--codex-home))
          lines)
    (push (format "toml.el available: %s"
                  (if (require 'toml nil t) "yes" "no"))
          lines)
    (nreverse lines)))

;;;###autoload
(defun cc-switch-diagnose ()
  "Show sanitized cc-switch.el diagnostics."
  (interactive)
  (let ((text (string-join (cc-switch--diagnose-lines) "\n")))
    (if (called-interactively-p 'interactive)
        (with-current-buffer (get-buffer-create "*cc-switch diagnose*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert text "\n")
            (special-mode))
          (display-buffer (current-buffer)))
      text)))

;;; Dashboard UI

(defvar-local cc-switch--dashboard-error nil
  "Last dashboard refresh error for the current buffer.")

(defun cc-switch--display-value (value &optional fallback)
  "Return VALUE as a non-empty display string, or FALLBACK."
  (let ((text (cond
               ((null value) nil)
               ((stringp value) (string-trim value))
               (t (format "%s" value)))))
    (if (and text (not (string-empty-p text)))
        text
      (or fallback "-"))))

(defun cc-switch--bool-label (value)
  "Return yes or no for VALUE."
  (if value "yes" "no"))

(defun cc-switch--ok-label (value)
  "Return an ok/missing status label for VALUE."
  (if value
      (propertize "ok" 'face 'success)
    (propertize "missing" 'face 'warning)))

(defun cc-switch--short-path (path)
  "Return PATH abbreviated for dashboard display."
  (abbreviate-file-name path))

(defun cc-switch--status-field (label value &optional width)
  "Return a dashboard status field with LABEL and VALUE.
When WIDTH is non-nil, right-pad the result to WIDTH."
  (let ((text (format "%s %s"
                      (propertize label 'face 'shadow)
                      value)))
    (if width
        (format (format "%%-%ds" width) text)
      text)))

(defun cc-switch--dashboard-status-row (name current live proxy &optional extra)
  "Return a compact dashboard status row.
NAME is the app or resource name.  CURRENT is the current provider or
resource path.  LIVE is the primary live path or status.  PROXY is the
proxy or access status.  EXTRA is optional trailing status text."
  (format "  %-8s current %-18s config %-30s %-12s %s"
          name
          current
          live
          proxy
          (or extra "")))

(defun cc-switch--truncate-cell (value width &optional fallback)
  "Return VALUE as a table cell no wider than WIDTH.
FALLBACK is used for empty values.  The full value is kept in
`help-echo' when truncation happens."
  (let ((text (cc-switch--display-value value fallback)))
    (if (> (string-width text) width)
        (propertize
         (truncate-string-to-width text width nil nil "...")
         'help-echo text)
      text)))

(defun cc-switch--dashboard-live-path (app)
  "Return APP primary live config path."
  (pcase app
    ("claude" (cc-switch--claude-settings-path))
    ("codex" (cc-switch--codex-config-path))))

(defun cc-switch--dashboard-live-dir (app)
  "Return APP live config directory."
  (pcase app
    ("claude" (cc-switch--claude-config-dir))
    ("codex" (cc-switch--codex-home))))

(defun cc-switch--safe-current-provider-id (app)
  "Return APP current provider id as display text."
  (condition-case err
      (or (cc-switch--current-provider-id app) "<none>")
    (error (format "error: %s" (error-message-string err)))))

(defun cc-switch--safe-proxy-status (app)
  "Return APP proxy status as display text."
  (condition-case err
      (or (cc-switch--proxy-blocking-reason app) "no")
    (error (format "error: %s" (error-message-string err)))))

(defun cc-switch--dashboard-db-status-line ()
  "Return the dashboard database status line."
  (let ((db-path (cc-switch--db-path))
        (legacy-path (cc-switch--legacy-config-path)))
    (format "  %-8s %-32s %-12s %-18s %s"
            "DB"
            (cc-switch--short-path db-path)
            (format "exists %s" (cc-switch--ok-label (file-exists-p db-path)))
            (format "access %s"
                    (if (and (file-readable-p db-path)
                             (file-writable-p db-path))
                        (propertize "ok" 'face 'success)
                      (propertize "limited" 'face 'warning)))
            (format "legacy %s" (cc-switch--bool-label (file-exists-p legacy-path))))))

(defun cc-switch--dashboard-app-status-line (app)
  "Return a dashboard status line for APP."
  (let* ((live-dir (cc-switch--dashboard-live-dir app))
         (live-path (cc-switch--dashboard-live-path app))
         (proxy (cc-switch--safe-proxy-status app))
         (codex-extra
          (and (string-equal app "codex")
               (format "toml %s"
                       (cc-switch--ok-label (require 'toml nil t))))))
    (cc-switch--dashboard-status-row
     (capitalize app)
     (cc-switch--safe-current-provider-id app)
     (cc-switch--short-path live-path)
     (if (string-equal proxy "no")
         (propertize "proxy ok" 'face 'success)
       (propertize proxy 'face 'warning))
     (string-join
      (delq nil
            (list (format "dir %s"
                          (cc-switch--ok-label (file-directory-p live-dir)))
                  codex-extra))
      ", "))))

(defun cc-switch--dashboard-status-lines ()
  "Return dashboard preamble lines."
  (append
   (list (propertize "cc-switch" 'face 'bold)
         (propertize
          "[?] menu   [g] refresh   [RET] details   [s] switch   [e] export   [q] quit"
          'face 'shadow)
         ""
         (propertize "Status" 'face 'bold)
         (cc-switch--dashboard-db-status-line))
   (mapcar #'cc-switch--dashboard-app-status-line cc-switch--supported-apps)
   (when cc-switch--dashboard-error
     (list (propertize
            (format "Refresh error: %s" cc-switch--dashboard-error)
            'face 'error)))
   (list ""
         (propertize "Providers" 'face 'bold))))

(defun cc-switch--dashboard-insert-status ()
  "Insert dashboard status lines at the top of the current buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (dolist (line (cc-switch--dashboard-status-lines))
        (insert line "\n"))
      (insert "\n"))))

(defun cc-switch--remove-fake-header-overlays ()
  "Remove `tabulated-list' fake header overlays from the dashboard.
Those overlays can inherit underline-heavy theme styling and, after the
dashboard preamble is inserted, may visually affect more than the table
header."
  (remove-overlays (point-min) (point-max)
                   'face 'tabulated-list-fake-header))

(defun cc-switch--dashboard-entry (provider)
  "Return a tabulated-list entry for PROVIDER."
  (let* ((app (cc-switch--provider-app provider))
         (provider-id (cc-switch--provider-id provider))
         (current (cc-switch--provider-current provider))
         (snippet (condition-case nil
                      (cc-switch--common-config-snippet app)
                    (error nil)))
         (proxy (cc-switch--safe-proxy-status app))
         (name (cc-switch--truncate-cell
                (cc-switch--provider-name provider) 30))
         (category (cc-switch--truncate-cell
                    (cc-switch--provider-category provider) 12))
         (id-cell (cc-switch--truncate-cell provider-id 22))
         (current-cell (if current
                           (propertize "*" 'face 'success)
                         ""))
         (tags
          (delq nil
                (list
                 (and (cc-switch--provider-uses-common-config-p provider snippet)
                      (propertize "common" 'face 'font-lock-constant-face))
                 (and (not (string-equal proxy "no"))
                      (propertize "proxy-blocked" 'face 'warning))
                 (and (string-equal app "codex")
                      (cc-switch--codex-official-provider-p provider)
                      (propertize "auth" 'face 'font-lock-keyword-face)))))
         (tag-cell (string-join tags " ")))
    (list (cons app provider-id)
          (vector current-cell
                  app
                  (if current (propertize name 'face 'bold) name)
                  id-cell
                  category
                  tag-cell))))

(defun cc-switch--dashboard-entries ()
  "Return all provider entries for the dashboard."
  (setq cc-switch--dashboard-error nil)
  (condition-case err
      (mapcan (lambda (app)
                (mapcar #'cc-switch--dashboard-entry
                        (cc-switch--providers app)))
              cc-switch--supported-apps)
    (error
     (setq cc-switch--dashboard-error (error-message-string err))
     nil)))

(defun cc-switch--setup-tabulated-list ()
  "Install cc-switch dashboard table settings in the current buffer."
  (setq tabulated-list-format
        [("Cur" 3 nil)
         ("App" 8 nil)
         ("Provider" 30 nil)
         ("ID" 22 nil)
         ("Kind" 12 nil)
         ("Tags" 24 nil)])
  (setq tabulated-list-use-header-line nil)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun cc-switch--goto-first-dashboard-entry ()
  "Move point to the first provider row when possible."
  (goto-char (point-min))
  (while (and (not (eobp))
              (not (tabulated-list-get-id)))
    (forward-line 1)))

(defun cc-switch-refresh ()
  "Refresh the current cc-switch dashboard."
  (interactive)
  (unless (derived-mode-p 'cc-switch-mode)
    (user-error "Not in a cc-switch dashboard buffer"))
  (cc-switch--setup-tabulated-list)
  (setq tabulated-list-entries (cc-switch--dashboard-entries))
  (tabulated-list-print t)
  (cc-switch--dashboard-insert-status)
  (cc-switch--remove-fake-header-overlays)
  (unless (tabulated-list-get-id)
    (cc-switch--goto-first-dashboard-entry)))

(defun cc-switch--refresh-dashboard-buffers ()
  "Refresh all open cc-switch dashboard buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'cc-switch-mode)
        (cc-switch-refresh)))))

(defun cc-switch--provider-reference-at-point (&optional noerror)
  "Return the provider reference at point as (APP . PROVIDER-ID).
When NOERROR is non-nil, return nil instead of signaling."
  (let ((id (and (derived-mode-p 'cc-switch-mode)
                 (tabulated-list-get-id))))
    (if (and (consp id)
             (member (car id) cc-switch--supported-apps)
             (stringp (cdr id)))
        id
      (unless noerror
        (user-error "No provider row at point")))))

(defun cc-switch--read-provider-reference (&optional prompt)
  "Read a provider reference, or use the provider row at point.
PROMPT is used for provider selection when no row is available."
  (or (cc-switch--provider-reference-at-point t)
      (let* ((app (cc-switch--read-app))
             (provider-id (cc-switch--read-provider-id app prompt)))
        (cons app provider-id))))

(defun cc-switch--show-text-buffer (buffer-name lines)
  "Show BUFFER-NAME containing LINES in `special-mode'."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (line lines)
        (insert line "\n"))
      (special-mode))
    (display-buffer (current-buffer))))

(defun cc-switch--provider-details-lines (app provider)
  "Return secret-safe detail lines for APP PROVIDER."
  (let* ((snippet (condition-case nil
                      (cc-switch--common-config-snippet app)
                    (error nil)))
         (paths (cc-switch--live-paths-for-switch app provider))
         (notes (cc-switch--display-value
                 (cc-switch--provider-notes provider) ""))
         (lines
          (list
           "Provider"
           ""
           (format "App: %s" app)
           (format "Name: %s" (cc-switch--provider-name provider))
           (format "ID: %s" (cc-switch--provider-id provider))
           (format "Category: %s"
                   (cc-switch--display-value
                    (cc-switch--provider-category provider)))
           (format "Website: %s"
                   (cc-switch--display-value
                    (cc-switch--provider-website-url provider)))
           (format "Current: %s"
                   (cc-switch--bool-label
                    (cc-switch--provider-current provider)))
           (format "Failover queue: %s"
                   (cc-switch--bool-label
                    (cc-switch--provider-in-failover-queue provider)))
           (format "Sort index: %s"
                   (cc-switch--display-value
                    (cc-switch--provider-sort-index provider)))
           (format "Common config: %s"
                   (cc-switch--bool-label
                    (cc-switch--provider-uses-common-config-p
                     provider snippet)))
           (format "Codex official: %s"
                   (if (string-equal app "codex")
                       (cc-switch--bool-label
                        (cc-switch--codex-official-provider-p provider))
                     "-"))
           ""
           "Writes:")))
    (setq lines
          (append lines
                  (mapcar (lambda (path) (format "- %s" path)) paths)
                  (list ""
                        "Hidden: settings_config, auth fields, API keys, and tokens")))
    (when (not (string-empty-p notes))
      (setq lines
            (append lines
                    (list ""
                          "Notes:"
                          notes))))
    lines))

(defun cc-switch-provider-details ()
  "Show secret-safe details for the provider at point or a chosen provider."
  (interactive)
  (let* ((ref (cc-switch--read-provider-reference "Show provider: "))
         (app (car ref))
         (provider-id (cdr ref))
         (provider (cc-switch--provider-by-id app provider-id)))
    (cc-switch--show-text-buffer
     "*cc-switch provider*"
     (cc-switch--provider-details-lines app provider))))

(defun cc-switch--confirm-risky-switch (app provider)
  "Ask for confirmation before risky APP PROVIDER switches."
  (when (and (string-equal app "codex")
             (cc-switch--codex-official-provider-p provider)
             (not (yes-or-no-p
                   (format "Switching to official Codex provider %s may update auth.json. Continue? "
                           (cc-switch--provider-name provider)))))
    (user-error "Cancelled")))

(defun cc-switch--switch-provider-from-ui (app provider-id)
  "Switch APP to PROVIDER-ID from dashboard-style UI."
  (setq app (cc-switch--normalize-app app))
  (let ((provider (cc-switch--provider-by-id app provider-id)))
    (if (cc-switch--provider-current provider)
        (message "%s already uses %s [%s]"
                 app
                 (cc-switch--provider-name provider)
                 provider-id)
      (cc-switch--confirm-risky-switch app provider)
      (setq provider (cc-switch--switch-provider app provider-id))
      (cc-switch--refresh-dashboard-buffers)
      (message "Switched %s to %s [%s]"
               app
               (cc-switch--provider-name provider)
               provider-id))
    provider))

(defun cc-switch-switch-provider-at-point ()
  "Switch to the provider on the current dashboard row."
  (interactive)
  (let ((ref (cc-switch--provider-reference-at-point)))
    (cc-switch--switch-provider-from-ui (car ref) (cdr ref))))

(defun cc-switch-switch-provider ()
  "Choose an app and provider, then switch using dashboard semantics."
  (interactive)
  (let* ((app (cc-switch--read-app))
         (provider-id (cc-switch--read-provider-id app "Switch to provider: ")))
    (cc-switch--switch-provider-from-ui app provider-id)))

(defun cc-switch-switch-claude-provider ()
  "Choose a Claude provider, then switch using dashboard semantics."
  (interactive)
  (cc-switch--switch-provider-from-ui
   "claude"
   (cc-switch--read-provider-id "claude" "Switch Claude to: ")))

(defun cc-switch-switch-codex-provider ()
  "Choose a Codex provider, then switch using dashboard semantics."
  (interactive)
  (cc-switch--switch-provider-from-ui
   "codex"
   (cc-switch--read-provider-id "codex" "Switch Codex to: ")))

(defun cc-switch-export-provider-at-point (&optional output)
  "Export the Claude provider at point to OUTPUT.
Interactively, export to ./.claude/settings.local.json unless a prefix
argument is used, in which case read the output path."
  (interactive
   (let* ((ref (cc-switch--provider-reference-at-point))
          (default (expand-file-name ".claude/settings.local.json"
                                     default-directory))
          (output (if current-prefix-arg
                      (read-file-name "Export to: "
                                      (file-name-directory default)
                                      default nil
                                      (file-name-nondirectory default))
                    default)))
     (unless (string-equal (car ref) "claude")
       (user-error "Only Claude providers can be exported"))
     (list output)))
  (let* ((ref (cc-switch--provider-reference-at-point))
         (app (car ref))
         (provider-id (cdr ref)))
    (unless (string-equal app "claude")
      (user-error "Only Claude providers can be exported"))
    (setq output
          (or output
              (expand-file-name ".claude/settings.local.json"
                                default-directory)))
    (cc-switch-provider-export provider-id output)
    (message "Exported Claude provider %s to %s" provider-id output)
    output))

(defun cc-switch-open-live-config ()
  "Open the live config file for the provider row at point or a chosen app."
  (interactive)
  (let* ((ref (or (cc-switch--provider-reference-at-point t)
                  (cons (cc-switch--read-app) nil)))
         (path (cc-switch--dashboard-live-path (car ref))))
    (find-file path)))

(defun cc-switch-open-backup ()
  "Open the single-file backup for the provider row at point or a chosen app."
  (interactive)
  (let* ((ref (or (cc-switch--provider-reference-at-point t)
                  (cons (cc-switch--read-app) nil)))
         (path (cc-switch--backup-path
                (cc-switch--dashboard-live-path (car ref)))))
    (unless (file-exists-p path)
      (user-error "Backup does not exist: %s" path))
    (find-file path)))

(transient-define-prefix cc-switch-menu ()
  "cc-switch command menu."
  ["View"
   [("g" "Refresh" cc-switch-refresh)
    ("RET" "Provider details" cc-switch-provider-details)
    ("d" "Diagnose" cc-switch-diagnose)]]
  ["Switch"
   [("s" "Switch row" cc-switch-switch-provider-at-point)
    ("S" "Choose provider" cc-switch-switch-provider)
    ("c" "Claude provider" cc-switch-switch-claude-provider)
    ("x" "Codex provider" cc-switch-switch-codex-provider)]]
  ["Export"
   [("e" "Export row" cc-switch-export-provider-at-point)
    ("E" "Choose Claude export" cc-switch-provider-export)]]
  ["Files"
   [("o" "Open live config" cc-switch-open-live-config)
    ("b" "Open backup" cc-switch-open-backup)]])

(defvar cc-switch-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "?") #'cc-switch-menu)
    (define-key map (kbd "g") #'cc-switch-refresh)
    (define-key map (kbd "RET") #'cc-switch-provider-details)
    (define-key map (kbd "s") #'cc-switch-switch-provider-at-point)
    (define-key map (kbd "S") #'cc-switch-switch-provider)
    (define-key map (kbd "e") #'cc-switch-export-provider-at-point)
    (define-key map (kbd "d") #'cc-switch-diagnose)
    (define-key map (kbd "o") #'cc-switch-open-live-config)
    (define-key map (kbd "b") #'cc-switch-open-backup)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `cc-switch-mode'.")

(defvar cc-switch-mode-syntax-table
  (make-syntax-table)
  "Syntax table for `cc-switch-mode'.")

(defun cc-switch--evil-normalize-keymaps ()
  "Refresh Evil keymaps for `cc-switch-mode'."
  (when (fboundp 'evil-normalize-keymaps)
    (evil-normalize-keymaps)))

(with-eval-after-load 'evil
  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map cc-switch-mode-map 'normal))
  (add-hook 'cc-switch-mode-hook #'cc-switch--evil-normalize-keymaps))

(define-derived-mode cc-switch-mode tabulated-list-mode "cc-switch"
  "Major mode for the cc-switch dashboard."
  (cc-switch--setup-tabulated-list)
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (cc-switch-refresh))))

;;;###autoload
(defun cc-switch ()
  "Open the cc-switch dashboard."
  (interactive)
  (let ((buffer (get-buffer-create cc-switch-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'cc-switch-mode)
        (cc-switch-mode))
      (cc-switch-refresh))
    (pop-to-buffer buffer)))

(provide 'cc-switch)

;;; cc-switch.el ends here
