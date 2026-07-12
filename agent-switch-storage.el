;;; agent-switch-storage.el --- Versioned JSON storage and file safety -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Versioned per-Profile JSON storage, state persistence, secret references,
;; optimistic concurrency, and atomic file helpers for agent-switch.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'rx)
(require 'subr-x)
(require 'agent-switch-core)

(declare-function agent-switch-find-profile
                  "agent-switch-operations" (client-id profile-id &optional noerror))
(declare-function agent-switch-delete-managed-profile
                  "agent-switch-operations" (profile))

(defcustom agent-switch-directory
  (expand-file-name "agent-switch/" user-emacs-directory)
  "Directory containing managed profiles and state."
  :type 'directory
  :group 'agent-switch)

(defcustom agent-switch-authinfo-file
  (expand-file-name "~/.authinfo.gpg")
  "Authinfo file used for managed secret references and command delivery."
  :type 'file
  :group 'agent-switch)

(defconst agent-switch-storage-schema-version 1
  "Current Profile JSON schema version.")

(defconst agent-switch-state-schema-version 2
  "Current state.json schema version.")

(cl-defstruct (agent-switch-file-state
               (:constructor agent-switch--make-file-state))
  path exists-p content hash)

(cl-defstruct (agent-switch-state-record
               (:constructor agent-switch--make-state-record))
  data hash error)

(defun agent-switch--directory ()
  "Return normalized `agent-switch-directory'."
  (file-name-as-directory (expand-file-name agent-switch-directory)))

(defun agent-switch-profiles-directory (&optional client-id)
  "Return profiles directory, optionally scoped to CLIENT-ID."
  (let ((root (expand-file-name "profiles/" (agent-switch--directory))))
    (if client-id
        (expand-file-name
         (file-name-as-directory
          (agent-switch--string-id client-id "client"))
         root)
      root)))

(defun agent-switch-state-path ()
  "Return the state JSON path."
  (expand-file-name "state.json" (agent-switch--directory)))

(defun agent-switch-profile-path (client-id profile-id)
  "Return managed profile path for CLIENT-ID and PROFILE-ID."
  (expand-file-name
   (concat (agent-switch--string-id profile-id "profile") ".json")
   (agent-switch-profiles-directory client-id)))

(defun agent-switch--read-file-text (path)
  "Read and decode PATH as text."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun agent-switch--read-file-bytes (path)
  "Read PATH literally and return its exact bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun agent-switch-content-hash (content)
  "Return a SHA-256 hash for CONTENT."
  (secure-hash 'sha256 content))

(defun agent-switch-capture-file (path)
  "Capture PATH content and hash for optimistic writes or rollback."
  (let ((exists-p (file-exists-p path)))
    (if exists-p
        (let ((content (agent-switch--read-file-bytes path)))
          (agent-switch--make-file-state
           :path path :exists-p t :content content
           :hash (agent-switch-content-hash content)))
      (agent-switch--make-file-state
       :path path :exists-p nil :content nil :hash :missing))))

(defun agent-switch--current-file-hash (path)
  "Return current hash for PATH, or `:missing'."
  (if (file-exists-p path)
      (agent-switch-content-hash (agent-switch--read-file-bytes path))
    :missing))

(defun agent-switch--assert-file-hash (path expected-hash)
  "Signal a conflict unless PATH still has EXPECTED-HASH."
  (unless (equal (agent-switch--current-file-hash path) expected-hash)
    (signal 'agent-switch-conflict
            (list (format "File changed externally: %s"
                          (abbreviate-file-name path))))))

(defun agent-switch-write-text-atomic (path text expected-hash &optional create-parent)
  "Atomically write TEXT to PATH if it still has EXPECTED-HASH.
When CREATE-PARENT is non-nil, create the parent directory."
  (let ((directory (file-name-directory path)))
    (when create-parent
      (make-directory directory t))
    (unless (file-directory-p directory)
      (signal 'agent-switch-error
              (list (format "Parent directory does not exist: %s"
                            (abbreviate-file-name directory)))))
    (agent-switch--assert-file-hash path expected-hash)
    (let ((temporary
           (make-temp-file
            (expand-file-name
             (concat "." (file-name-nondirectory path) ".tmp-") directory))))
      (unwind-protect
          (progn
            (let ((coding-system-for-write
                   (if (multibyte-string-p text) 'utf-8-unix 'no-conversion)))
              (with-temp-file temporary (insert text)))
            (agent-switch--assert-file-hash path expected-hash)
            (rename-file temporary path t)
            (agent-switch--current-file-hash path))
        (when (file-exists-p temporary)
          (ignore-errors (delete-file temporary)))))))

(defun agent-switch-delete-file-optimistic (path expected-hash)
  "Delete PATH only if it still has EXPECTED-HASH."
  (agent-switch--assert-file-hash path expected-hash)
  (when (file-exists-p path)
    (delete-file path)))

(defun agent-switch-backup-file (path)
  "Create and return a timestamped backup of PATH, or nil if absent."
  (when (file-exists-p path)
    (let* ((stamp (format-time-string "%Y%m%dT%H%M%S"))
           (backup (format "%s.agent-switch.bak.%s" path stamp))
           (candidate backup)
           (counter 1))
      (while (file-exists-p candidate)
        (setq candidate (format "%s.%d" backup counter)
              counter (1+ counter)))
      (copy-file path candidate nil t t)
      candidate)))

(defun agent-switch-restore-file (state)
  "Restore file STATE captured by `agent-switch-capture-file'."
  (let ((path (agent-switch-file-state-path state)))
    (if (agent-switch-file-state-exists-p state)
        (agent-switch-write-text-atomic
         path (agent-switch-file-state-content state)
         (agent-switch-file-state-hash state) t)
      (agent-switch-delete-file-optimistic
       path (agent-switch-file-state-hash state)))))

(defun agent-switch-parse-json (text &optional context)
  "Parse JSON TEXT as hash-table data.
CONTEXT is included in sanitized parse errors."
  (condition-case nil
      (json-parse-string text
                         :object-type 'hash-table
                         :array-type 'array
                         :null-object agent-switch-json-null
                         :false-object agent-switch-json-false)
    (json-parse-error
     (signal 'agent-switch-validation-error
             (list (format "Invalid JSON in %s" (or context "data")))))))

(defun agent-switch-json-serialize (value)
  "Serialize JSON VALUE in a stable human-readable form."
  (with-temp-buffer
    (insert (json-serialize value
                            :null-object agent-switch-json-null
                            :false-object agent-switch-json-false))
    (json-pretty-print-buffer)
    (unless (bolp) (insert "\n"))
    (buffer-string)))

(defun agent-switch-json-copy (value)
  "Return a deep copy of JSON VALUE."
  (cond
   ((hash-table-p value)
    (let ((copy (make-hash-table :test #'equal)))
      (maphash (lambda (key child)
                 (puthash key (agent-switch-json-copy child) copy))
               value)
      copy))
   ((vectorp value)
    (vconcat (mapcar #'agent-switch-json-copy (append value nil))))
   ((consp value) (mapcar #'agent-switch-json-copy value))
   (t value)))

(defun agent-switch-json-deep-merge (target patch)
  "Deeply merge JSON PATCH into TARGET and return a copy."
  (if (and (hash-table-p target) (hash-table-p patch))
      (let ((result (agent-switch-json-copy target)))
        (maphash
         (lambda (key value)
           (puthash key
                    (agent-switch-json-deep-merge
                     (gethash key result) value)
                    result))
         patch)
        result)
    (agent-switch-json-copy patch)))

(defun agent-switch--auth-source-matches (reference)
  "Return auth-source matches for REFERENCE without stale search results."
  (let* ((auth-source-do-cache nil)
         (auth-sources (list (expand-file-name agent-switch-authinfo-file)))
         (authinfo (gethash "authinfo" reference))
         (machine (gethash "machine" authinfo))
         (login (gethash "login" authinfo))
         (port (gethash "port" authinfo)))
    (apply #'auth-source-search
           (append (list :host machine :max 1 :require '(:secret))
                   (when login (list :user login))
                   (when port (list :port port))))))

(defun agent-switch--auth-source-reference-available-p (reference)
  "Return non-nil when auth-source can match REFERENCE with a secret."
  (condition-case nil
      (not (null (agent-switch--auth-source-matches reference)))
    (error nil)))

(defun agent-switch--secret-references-available-p (value)
  "Return non-nil when every secret reference below VALUE is available."
  (cond
   ((agent-switch-secret-reference-p value)
    (agent-switch--auth-source-reference-available-p value))
   ((hash-table-p value)
    (catch 'missing
      (maphash
       (lambda (_key child)
         (unless (agent-switch--secret-references-available-p child)
           (throw 'missing nil)))
       value)
      t))
   ((vectorp value)
    (cl-loop for child across value
             always (agent-switch--secret-references-available-p child)))
   ((consp value)
    (cl-every #'agent-switch--secret-references-available-p value))
   (t t)))

(defun agent-switch--resolve-secret-reference (reference)
  "Resolve secret REFERENCE or signal a sanitized error."
  (let ((source (gethash "source" reference)))
    (pcase source
      ("auth-source"
       (let* ((authinfo (gethash "authinfo" reference))
              (machine (gethash "machine" authinfo))
              (match (car (agent-switch--auth-source-matches reference)))
              (secret (plist-get match :secret))
              (value (if (functionp secret) (funcall secret) secret)))
         (unless (and (stringp value) (not (string-empty-p value)))
           (signal 'agent-switch-error
                   (list (format "No auth-source secret found for %s"
                                 machine))))
         value))
      (_ (signal 'agent-switch-validation-error
                 '("Only auth-source secret references are supported"))))))

(defun agent-switch--resolve-secrets (value secrets)
  "Return (RESOLVED . SECRETS) for JSON VALUE and accumulated SECRETS."
  (cond
   ((agent-switch-secret-reference-p value)
    (if (equal (gethash "delivery" value) "command")
        (if (agent-switch--auth-source-reference-available-p value)
            (cons (agent-switch-json-copy value) secrets)
          (let* ((authinfo (gethash "authinfo" value))
                 (machine (gethash "machine" authinfo)))
            (signal 'agent-switch-error
                    (list (format "No auth-source secret found for %s"
                                  machine)))))
      (let ((secret (agent-switch--resolve-secret-reference value)))
        (cons secret (cons secret secrets)))))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test #'equal))
          (values secrets))
      (maphash
       (lambda (key child)
         (pcase-let ((`(,resolved . ,new-values)
                      (agent-switch--resolve-secrets child values)))
           (setq values new-values)
           (puthash key resolved copy)))
       value)
      (cons copy values)))
   ((vectorp value)
    (let ((copy (make-vector (length value) nil))
          (values secrets))
      (dotimes (index (length value))
        (pcase-let ((`(,resolved . ,new-values)
                     (agent-switch--resolve-secrets (aref value index) values)))
          (setq values new-values)
          (aset copy index resolved)))
      (cons copy values)))
   ((consp value)
    (let (copy (values secrets))
      (dolist (child value)
        (pcase-let ((`(,resolved . ,new-values)
                     (agent-switch--resolve-secrets child values)))
          (setq values new-values)
          (push resolved copy)))
      (cons (nreverse copy) values)))
   (t (cons value secrets))))

(defun agent-switch-resolve-profile-secrets (profile)
  "Return (RESOLVED-PROFILE . SECRET-VALUES) for PROFILE."
  (pcase-let ((`(,payload . ,secrets)
               (agent-switch--resolve-secrets
                (agent-switch-profile-payload profile) nil)))
    (cons (let ((copy (copy-agent-switch-profile profile)))
            (setf (agent-switch-profile-payload copy) payload)
            copy)
          secrets)))

(defun agent-switch--profile-json (profile)
  "Return versioned JSON object for managed PROFILE."
  (let ((object (make-hash-table :test #'equal)))
    (puthash "schema_version" agent-switch-storage-schema-version object)
    (puthash "id" (agent-switch-profile-id profile) object)
    (puthash "client" (agent-switch-profile-client-id profile) object)
    (puthash "name" (agent-switch-profile-name profile) object)
    (puthash "payload_schema_version"
             (or (agent-switch-profile-payload-version profile) 1) object)
    (puthash "payload" (agent-switch-profile-payload profile) object)
    (when (agent-switch-profile-setup-required-p profile)
      (puthash "setup_required" t object)
      (puthash "warnings"
               (vconcat (or (agent-switch-profile-warnings profile) nil))
               object))
    object))

(defun agent-switch--profile-from-json (path client-id object hash)
  "Build a managed profile from PATH, CLIENT-ID, OBJECT, and HASH."
  (let ((version (gethash "schema_version" object))
        (id (gethash "id" object))
        (stored-client (gethash "client" object))
        (name (gethash "name" object))
        (payload-version (or (gethash "payload_schema_version" object) 1))
        (payload (gethash "payload" object))
        (setup-required-p (eq (gethash "setup_required" object) t))
        (warnings (append (or (gethash "warnings" object) []) nil)))
    (unless (equal version agent-switch-storage-schema-version)
      (signal 'agent-switch-validation-error
              (list (format "Unsupported profile schema version: %S" version))))
    (setq id (agent-switch--string-id id "profile"))
    (unless (equal (file-name-base path) id)
      (signal 'agent-switch-validation-error
              '("Profile filename does not match its ID")))
    (unless (equal stored-client client-id)
      (signal 'agent-switch-validation-error
              '("Profile belongs to another client")))
    (agent-switch-validate-profile-base
     (agent-switch--make-profile
      :id id :client-id client-id :name name
      :payload payload :ownership 'managed :source path :source-hash hash
      :valid-p t :payload-version payload-version
      :setup-required-p setup-required-p :warnings warnings)
     t)))

(defun agent-switch--invalid-profile (path client-id error-value hash)
  "Return an invalid profile for PATH and CLIENT-ID.
ERROR-VALUE is sanitized for display and HASH records the source content."
  (agent-switch--make-profile
   :id (file-name-base path)
   :client-id client-id
   :name (file-name-base path)
   :payload (make-hash-table :test #'equal)
   :ownership 'managed
   :source path
   :source-hash hash
   :valid-p nil
   :error (agent-switch--safe-error-message error-value)))

(defun agent-switch-load-managed-profiles (client-id)
  "Load managed profiles for CLIENT-ID, isolating per-file errors."
  (setq client-id (agent-switch--string-id client-id "client"))
  (let ((directory (agent-switch-profiles-directory client-id))
        profiles)
    (when (file-directory-p directory)
      (dolist (path (directory-files directory t "\\.json\\'" t))
        (let* ((text (agent-switch--read-file-text path))
               (hash (agent-switch-content-hash
                      (agent-switch--read-file-bytes path))))
          (push
           (condition-case error-value
               (agent-switch--profile-from-json
                path client-id
                (agent-switch-parse-json text (file-name-nondirectory path))
                hash)
             (error (agent-switch--invalid-profile
                     path client-id error-value hash)))
           profiles))))
    (sort profiles
          (lambda (left right)
            (string-lessp (agent-switch-profile-id left)
                          (agent-switch-profile-id right))))))

(defun agent-switch-save-profile (profile)
  "Validate and atomically save managed PROFILE."
  (unless (eq (agent-switch-profile-ownership profile) 'managed)
    (signal 'agent-switch-validation-error
            '("Only managed profiles can be saved")))
  (let* ((client-id (agent-switch--string-id
                     (agent-switch-profile-client-id profile) "client"))
         (id (agent-switch--string-id
              (agent-switch-profile-id profile) "profile"))
         (path (agent-switch-profile-path client-id id))
         (expected (or (agent-switch-profile-source-hash profile) :missing)))
    (unless (agent-switch-profile-payload-version profile)
      (setf (agent-switch-profile-payload-version profile)
            (agent-switch-adapter-payload-version
             (agent-switch-get-adapter
              (agent-switch-client-adapter-id
               (agent-switch-get-client client-id))))))
    (agent-switch-validate-profile-base profile t)
    (let ((hash (agent-switch-write-text-atomic
                 path
                 (agent-switch-json-serialize
                  (agent-switch--profile-json profile))
                 expected t)))
      (setf (agent-switch-profile-source profile) path
            (agent-switch-profile-source-hash profile) hash
            (agent-switch-profile-valid-p profile) t)
      profile)))

(defun agent-switch--empty-state ()
  "Return a new versioned state object."
  (let ((object (make-hash-table :test #'equal)))
    (puthash "schema_version" agent-switch-state-schema-version object)
    (puthash "selections" (make-hash-table :test #'equal) object)
    (puthash "initialized_clients" (make-hash-table :test #'equal) object)
    (puthash "unprotected_confirmed" [] object)
    (puthash "canonical_confirmations" (make-hash-table :test #'equal) object)
    object))

(defun agent-switch--migrate-state-v1 (old)
  "Return state schema v2 migrated from OLD schema v1 data."
  (let* ((new (agent-switch--empty-state))
         (selected (gethash "last_selected" old))
         (applied (gethash "applied_profiles" old))
         (sources (gethash "selection_sources" old))
         (selections (gethash "selections" new))
         (initialized (gethash "initialized_clients" new)))
    (when (hash-table-p selected)
      (maphash
       (lambda (client-id profile-id)
         (let* ((snapshot (and (hash-table-p applied)
                               (gethash client-id applied)))
                (selection (make-hash-table :test #'equal)))
           (puthash "profile_id" profile-id selection)
           (puthash "payload"
                    (and (hash-table-p snapshot)
                         (gethash "payload" snapshot))
                    selection)
           (puthash "source"
                    (or (and (hash-table-p sources)
                             (gethash client-id sources))
                        "applied")
                    selection)
           (puthash client-id selection selections)
           (puthash client-id t initialized)))
       selected))
    (dolist (key '("unprotected_confirmed" "canonical_confirmations"))
      (when-let* ((value (gethash key old)))
        (puthash key (agent-switch-json-copy value) new)))
    new))

(defun agent-switch-read-state ()
  "Read state and return an `agent-switch-state-record'."
  (let ((path (agent-switch-state-path)))
    (if (not (file-exists-p path))
        (agent-switch--make-state-record
         :data (agent-switch--empty-state) :hash :missing)
      (let* ((text (agent-switch--read-file-text path))
             (hash (agent-switch-content-hash
                    (agent-switch--read-file-bytes path))))
        (condition-case error-value
            (let* ((parsed (agent-switch-parse-json text "state.json"))
                   (version (gethash "schema_version" parsed))
                   (data
                    (cond
                     ((equal version agent-switch-state-schema-version) parsed)
                     ((equal version 1) (agent-switch--migrate-state-v1 parsed))
                     (t
                      (signal 'agent-switch-validation-error
                              '("Unsupported state schema version"))))))
              (agent-switch--make-state-record :data data :hash hash))
          (error
           (agent-switch--make-state-record
            :data (agent-switch--empty-state)
            :hash hash
            :error (agent-switch--safe-error-message error-value))))))))

(defun agent-switch-update-state (mutator)
  "Apply MUTATOR to state and atomically persist it."
  (let* ((record (agent-switch-read-state))
         (error-text (agent-switch-state-record-error record)))
    (when error-text
      (signal 'agent-switch-validation-error
              (list (concat "state.json is damaged; reset it before writing: "
                            error-text))))
    (let ((data (agent-switch-json-copy
                 (agent-switch-state-record-data record))))
      (funcall mutator data)
      (agent-switch-write-text-atomic
       (agent-switch-state-path)
       (agent-switch-json-serialize data)
       (agent-switch-state-record-hash record) t)
      data)))

(defun agent-switch-reset-state ()
  "Back up a damaged or valid state file and replace it with empty state."
  (interactive)
  (let* ((path (agent-switch-state-path))
         (expected (agent-switch--current-file-hash path))
         (backup (agent-switch-backup-file path)))
    (agent-switch-write-text-atomic
     path (agent-switch-json-serialize (agent-switch--empty-state))
     expected t)
    (when (called-interactively-p 'interactive)
      (message "Reset agent-switch state%s"
               (if backup
                   (format "; backup: %s" (abbreviate-file-name backup))
                 "")))
    backup))

(defun agent-switch-state-last-selected (client-id)
  "Return last selected Profile ID for CLIENT-ID."
  (let* ((data (agent-switch-state-record-data (agent-switch-read-state)))
         (selections (gethash "selections" data))
         (selection (and (hash-table-p selections)
                         (gethash client-id selections))))
    (and (hash-table-p selection) (gethash "profile_id" selection))))

(defun agent-switch-state-client-initialized-p (client-id)
  "Return non-nil when CLIENT-ID has completed first-run initialization."
  (setq client-id (agent-switch--string-id client-id "client"))
  (let* ((data (agent-switch-state-record-data (agent-switch-read-state)))
         (initialized (gethash "initialized_clients" data)))
    (and (hash-table-p initialized)
         (eq (gethash client-id initialized) t))))

(defun agent-switch--state-put-selection (data client-id profile source)
  "Put PROFILE selection for CLIENT-ID into state DATA with SOURCE."
  (let ((selection (make-hash-table :test #'equal))
        (selections (or (gethash "selections" data)
                        (let ((new (make-hash-table :test #'equal)))
                          (puthash "selections" new data)
                          new))))
    (puthash "profile_id" (agent-switch-profile-id profile) selection)
    (puthash "payload" (agent-switch-json-copy
                        (agent-switch-profile-payload profile)) selection)
    (puthash "source" source selection)
    (puthash client-id selection selections)))

(defun agent-switch-state-set-last-selected
    (client-id profile-id &optional profile-object source)
  "Record PROFILE-ID and its applied payload snapshot for CLIENT-ID."
  (agent-switch-update-state
   (lambda (data)
     (let ((profile (or profile-object
                        (agent-switch-find-profile client-id profile-id))))
       (agent-switch--state-put-selection
        data client-id profile (or source "applied"))))))

(defun agent-switch-state-finish-client-initialization
    (client-id &optional selected-profile)
  "Mark CLIENT-ID initialized and optionally select SELECTED-PROFILE.
The marker survives Profile deletion so first-run capture is not repeated."
  (setq client-id (agent-switch--string-id client-id "client"))
  (agent-switch-update-state
   (lambda (data)
     (let ((initialized
            (or (gethash "initialized_clients" data)
                (let ((new (make-hash-table :test #'equal)))
                  (puthash "initialized_clients" new data)
                  new))))
       (unless (hash-table-p initialized)
         (signal 'agent-switch-validation-error
                 '("state initialized_clients must be an object")))
       (puthash client-id t initialized)
       (when selected-profile
         (agent-switch--state-put-selection
          data client-id selected-profile "adopted"))))))

(defun agent-switch-state-selection (client-id)
  "Return structured selected Profile state for CLIENT-ID, or nil."
  (let* ((data (agent-switch-state-record-data (agent-switch-read-state)))
         (selections (gethash "selections" data)))
    (and (hash-table-p selections) (gethash client-id selections))))

(defun agent-switch-state-remove-profile (client-id profile-id)
  "Remove PROFILE-ID references for CLIENT-ID from state."
  (agent-switch-update-state
   (lambda (data)
     (let* ((selections (gethash "selections" data))
            (selection (and (hash-table-p selections)
                            (gethash client-id selections))))
       (when (and (hash-table-p selection)
                  (equal (gethash "profile_id" selection) profile-id))
         (remhash client-id selections))))))

(defun agent-switch-state-unprotected-confirmed-p (adapter-id)
  "Return non-nil if ADAPTER-ID activation risk was confirmed."
  (let* ((data (agent-switch-state-record-data (agent-switch-read-state)))
         (confirmed (gethash "unprotected_confirmed" data)))
    (member adapter-id (append confirmed nil))))

(defun agent-switch-state-confirm-unprotected (adapter-id)
  "Record confirmation for ADAPTER-ID without rollback support."
  (agent-switch-update-state
   (lambda (data)
     (let* ((old (append (gethash "unprotected_confirmed" data) nil))
            (new (vconcat (cl-adjoin adapter-id old :test #'equal))))
       (puthash "unprotected_confirmed" new data)))))

(defun agent-switch-state-canonical-confirmed-p (key hash)
  "Return non-nil when canonical rewrite KEY was confirmed for HASH."
  (let* ((data (agent-switch-state-record-data (agent-switch-read-state)))
         (table (gethash "canonical_confirmations" data)))
    (and (hash-table-p table) (equal (gethash key table) hash))))

(defun agent-switch-state-confirm-canonical (key hash)
  "Record canonical rewrite KEY confirmation for source HASH."
  (agent-switch-update-state
   (lambda (data)
     (let ((table (or (gethash "canonical_confirmations" data)
                      (let ((new (make-hash-table :test #'equal)))
                        (puthash "canonical_confirmations" new data)
                        new))))
       (puthash key hash table)))))

(provide 'agent-switch-storage)

;;; agent-switch-storage.el ends here
