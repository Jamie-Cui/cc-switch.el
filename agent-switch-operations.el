;;; agent-switch-operations.el --- Reusable profile operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Noninteractive application operations shared by the dashboard and Elisp
;; callers.  This layer owns mutation policy and cross-repository recovery;
;; it does not prompt, visit files, render buffers, or emit user messages.

;;; Code:

(require 'cl-lib)
(require 'agent-switch-core)
(require 'agent-switch-storage)

(defvar agent-switch--running-jobs (make-hash-table :test #'equal)
  "Mutating Client Jobs shared by all callers and dashboard buffers.")

(defvar agent-switch--discovery-cache (make-hash-table :test #'equal)
  "Asynchronous Adapter discovery results keyed by Client ID.")

(defun agent-switch--adapter-discovered-profiles (client)
  "Return cached Adapter-discovered Profiles for CLIENT."
  (let* ((adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (discover (agent-switch-adapter-callback adapter :discover))
         (client-id (agent-switch-client-id client))
         (cached (gethash client-id agent-switch--discovery-cache)))
    (when discover
      (pcase (plist-get cached :status)
        ('ready (plist-get cached :value))
        ('pending nil)
        ('error (signal 'agent-switch-error (list (plist-get cached :error))))
        (_
         (let ((result (funcall discover client nil)))
           (if (not (agent-switch-job-p result))
               result
             (puthash client-id (list :status 'pending :job result)
                      agent-switch--discovery-cache)
             (agent-switch-job-start
              result
              (lambda (profiles)
                (puthash client-id (list :status 'ready :value profiles)
                         agent-switch--discovery-cache)
                (run-hook-with-args 'agent-switch-data-changed-hook client-id))
              (lambda (error-value)
                (puthash client-id
                         (list :status 'error
                               :error (agent-switch--safe-error-message error-value))
                         agent-switch--discovery-cache)
                (run-hook-with-args 'agent-switch-data-changed-hook client-id)))
             nil)))))))

(defun agent-switch-profile-discovery-status (client-id)
  "Return cached asynchronous discovery status for CLIENT-ID, or nil."
  (setq client-id (agent-switch--string-id client-id "client"))
  (plist-get (gethash client-id agent-switch--discovery-cache) :status))

(defun agent-switch-invalidate-discovery (&optional client-id)
  "Invalidate asynchronous discovery cache for CLIENT-ID or all clients."
  (if client-id
      (progn
        (when-let* ((entry (gethash client-id agent-switch--discovery-cache))
                    (job (and (eq (plist-get entry :status) 'pending)
                              (plist-get entry :job))))
          (agent-switch-job-cancel job))
        (remhash client-id agent-switch--discovery-cache))
    (maphash (lambda (_id entry)
               (when-let* ((job (and (eq (plist-get entry :status) 'pending)
                                     (plist-get entry :job))))
                 (agent-switch-job-cancel job)))
             agent-switch--discovery-cache)
    (clrhash agent-switch--discovery-cache)))

(defun agent-switch-profiles (client-id)
  "Return all Profiles for CLIENT-ID with unique, deterministic identities."
  (let* ((client (agent-switch-get-client client-id))
         (profiles
          (mapcar
           (lambda (profile)
             (if (not (agent-switch-profile-valid-p profile))
                 profile
               (condition-case error-value
                   (agent-switch-validate-profile-base profile t)
                 (error
                  (let ((invalid (copy-agent-switch-profile profile)))
                    (setf (agent-switch-profile-valid-p invalid) nil
                          (agent-switch-profile-payload invalid)
                          (make-hash-table :test #'equal)
                          (agent-switch-profile-error invalid)
                          (agent-switch--safe-error-message error-value))
                    invalid)))))
           (append (agent-switch-load-managed-profiles client-id)
                   (agent-switch-external-profiles client-id)
                   (agent-switch--adapter-discovered-profiles client))))
         (by-id (make-hash-table :test #'equal)))
    (dolist (profile profiles)
      (let ((id (agent-switch-profile-id profile)))
        (puthash id (cons profile (gethash id by-id)) by-id)))
    (let (unique)
      (maphash
       (lambda (id matches)
         (push
          (if (cdr matches)
              (agent-switch--make-profile
               :id id :client-id (agent-switch-client-id client)
               :name id :payload (make-hash-table :test #'equal)
               :ownership 'conflict :source 'multiple :valid-p nil
               :error (format "Duplicate Profile ID %s for Client %s"
                              id (agent-switch-client-id client)))
            (car matches))
          unique))
       by-id)
      (sort unique
            (lambda (left right)
              (string-lessp (agent-switch-profile-id left)
                            (agent-switch-profile-id right)))))))

(defun agent-switch-find-profile (client-id profile-id &optional noerror)
  "Return CLIENT-ID PROFILE-ID, or nil with NOERROR."
  (or (cl-find profile-id (agent-switch-profiles client-id)
               :key #'agent-switch-profile-id :test #'equal)
      (unless noerror
        (signal 'agent-switch-error
                (list (format "Unknown profile %s/%s" client-id profile-id))))))

(defun agent-switch-operation-running-p (client-id)
  "Return non-nil when CLIENT-ID has a mutating operation in progress."
  (gethash (agent-switch--string-id client-id "client")
           agent-switch--running-jobs))

(defun agent-switch-ensure-client-idle (client)
  "Signal when CLIENT already has a mutating operation in progress."
  (when (agent-switch-operation-running-p (agent-switch-client-id client))
    (signal 'agent-switch-error
            (list (format "%s already has an operation in progress"
                          (agent-switch-client-name client))))))

(defun agent-switch--random-profile-id (client-id)
  "Return an unused short random Profile ID for CLIENT-ID."
  (let (candidate)
    (while
        (progn
          (setq candidate
                (concat
                 "p-"
                 (substring
                  (secure-hash
                   'sha256
                   (format "%s:%s:%s:%s"
                           client-id (float-time) (random) (emacs-pid)))
                  0 8)))
          (file-exists-p (agent-switch-profile-path client-id candidate))))
    candidate))

(defun agent-switch-create-managed-profile
    (client name payload &optional setup-required-p warnings)
  "Create and persist a managed Profile for CLIENT.
NAME and PAYLOAD are persisted without opening a buffer.  SETUP-REQUIRED-P and
WARNINGS describe an intentionally incomplete captured Profile."
  (agent-switch-ensure-client-idle client)
  (let* ((client-id (agent-switch-client-id client))
         (adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (profile (agent-switch--make-profile
                   :id (agent-switch--random-profile-id client-id)
                   :client-id client-id :name name :payload payload
                   :ownership 'managed :valid-p t
                   :payload-version
                   (agent-switch-adapter-payload-version adapter)
                   :setup-required-p setup-required-p
                   :warnings warnings)))
    (agent-switch-save-profile profile)))

(defun agent-switch--delete-profile-file-only (profile)
  "Delete PROFILE's managed file without changing selection state."
  (agent-switch-delete-file-optimistic
   (agent-switch-profile-source profile)
   (agent-switch-profile-source-hash profile)))

(defun agent-switch-adopt-capture (client name captured)
  "Persist CAPTURED current state as a managed Profile named NAME.
Complete captures become the selected state with `adopted' provenance.
Incomplete captures remain editable setup-required Profiles."
  (let* ((capture (agent-switch-normalize-capture-result captured))
         (complete-p (agent-switch-capture-result-complete-p capture))
         (profile
          (agent-switch-create-managed-profile
           client name (agent-switch-capture-result-payload capture)
           (not complete-p) (agent-switch-capture-result-warnings capture))))
    (when complete-p
      (condition-case error-value
          (agent-switch-state-set-last-selected
           (agent-switch-client-id client) (agent-switch-profile-id profile)
           profile "adopted")
        (error
         (ignore-errors (agent-switch--delete-profile-file-only profile))
         (signal (car error-value) (cdr error-value)))))
    profile))

(defun agent-switch--map-operation-result (result transform)
  "Apply TRANSFORM to direct or asynchronous RESULT."
  (if (not (agent-switch-job-p result))
      (funcall transform result)
    (agent-switch-job-create
     :canceler (lambda () (agent-switch-job-cancel result))
     :starter
     (lambda (resolve reject)
       (agent-switch-job-start
        result
        (lambda (value)
          (condition-case error-value
              (let ((mapped (funcall transform value)))
                (if (agent-switch-job-p mapped)
                    (agent-switch-job-start mapped resolve reject)
                  (funcall resolve mapped)))
            (error (funcall reject error-value))))
        reject)))))

(defun agent-switch-adopt-current (client name &optional current)
  "Capture CLIENT's CURRENT state and persist it as Profile NAME.
When CURRENT is nil, observe it through the Adapter.  Return a Profile or Job."
  (agent-switch-ensure-client-idle client)
  (let* ((adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (capture (agent-switch-adapter-callback adapter :capture-current)))
    (unless capture
      (signal 'agent-switch-validation-error
              (list (format "%s cannot capture current state"
                            (agent-switch-adapter-name adapter)))))
    (agent-switch--map-operation-result
     (or current (agent-switch-call client :current nil))
     (lambda (observed)
       (unless observed
         (signal 'agent-switch-validation-error
                 '("Client has no current configuration to adopt")))
       (agent-switch--map-operation-result
        (funcall capture client observed nil)
        (lambda (captured)
          (agent-switch-adopt-capture client name captured)))))))

(defun agent-switch-bootstrap-client (client current)
  "Capture CURRENT as CLIENT's first managed `default' Profile.
Return the Profile for a synchronous capture, or nil when automatic capture is
not available.  This writes only agent-switch Profile and state files; it does
not activate or rewrite CLIENT's live configuration."
  (agent-switch-ensure-client-idle client)
  (let* ((client-id (agent-switch-client-id client))
         (adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (capture (agent-switch-adapter-callback adapter :capture-current))
         (state (agent-switch-read-state)))
    (when (agent-switch-state-record-error state)
      (signal 'agent-switch-validation-error
              (list (concat "state.json is damaged; reset it before writing: "
                            (agent-switch-state-record-error state)))))
    (when (and current capture
               (not (agent-switch-state-client-initialized-p client-id)))
      (let ((captured (funcall capture client current nil)))
        (unless (agent-switch-job-p captured)
          (let* ((result (agent-switch-normalize-capture-result captured))
                 (complete-p (agent-switch-capture-result-complete-p result))
                 (profile
                  (agent-switch--make-profile
                   :id (agent-switch--random-profile-id client-id)
                   :client-id client-id :name "default"
                   :payload (agent-switch-capture-result-payload result)
                   :ownership 'managed :valid-p t
                   :payload-version
                   (agent-switch-adapter-payload-version adapter)
                   :setup-required-p (not complete-p)
                   :warnings (agent-switch-capture-result-warnings result))))
            (when (and complete-p
                       (not (agent-switch-profile-current-p
                             client profile current nil)))
              (signal 'agent-switch-validation-error
                      '("Captured Default does not match current state")))
            (agent-switch-save-profile profile)
            (condition-case error-value
                (progn
                  (agent-switch-state-finish-client-initialization
                   client-id (and complete-p profile))
                  profile)
              (error
               (ignore-errors (agent-switch--delete-profile-file-only profile))
               (signal (car error-value) (cdr error-value))))))))))

(defun agent-switch-apply-profile (client profile &optional allow-unprotected)
  "Return a tracked activation Job for CLIENT and PROFILE.
Signal unless rollback is available or ALLOW-UNPROTECTED is non-nil.  Callers
that set ALLOW-UNPROTECTED own the user or deployment policy decision."
  (agent-switch-ensure-client-idle client)
  (let* ((adapter (agent-switch-get-adapter
                   (agent-switch-client-adapter-id client)))
         (protected-p
          (and (agent-switch-adapter-capability-p adapter :snapshot)
               (agent-switch-adapter-capability-p adapter :rollback))))
    (unless (or protected-p allow-unprotected)
      (signal 'agent-switch-validation-error
              (list (format "%s has no automatic recovery; explicit approval is required"
                            (agent-switch-adapter-name adapter)))))
    (let* ((client-id (agent-switch-client-id client))
           (child (agent-switch-activation-job client profile))
           wrapper)
      (setq wrapper
            (agent-switch-job-create
             :canceler (lambda ()
                         (agent-switch-job-cancel child)
                         (remhash client-id agent-switch--running-jobs))
             :starter
             (lambda (resolve reject)
               (condition-case error-value
                   (progn
                     (agent-switch-ensure-client-idle client)
                     (puthash client-id wrapper agent-switch--running-jobs)
                     (agent-switch-job-start
                      child
                      (lambda (value)
                        (remhash client-id agent-switch--running-jobs)
                        (funcall resolve value))
                      (lambda (failure)
                        (remhash client-id agent-switch--running-jobs)
                        (funcall reject failure))))
                 (error
                  (remhash client-id agent-switch--running-jobs)
                  (funcall reject error-value))))))
      wrapper)))

(defun agent-switch-delete-managed-profile (profile)
  "Delete managed PROFILE and reconcile selection state.
Restore the Profile file if the state update fails."
  (unless (eq (agent-switch-profile-ownership profile) 'managed)
    (signal 'agent-switch-validation-error
            '("Only managed Profiles can be deleted")))
  (let* ((client-id (agent-switch-profile-client-id profile))
         (client (agent-switch-get-client client-id))
         (snapshot (agent-switch-capture-file
                    (agent-switch-profile-source profile))))
    (agent-switch-ensure-client-idle client)
    ;; Preflight damaged state before deleting the Profile file.
    (when-let* ((state-error
                 (agent-switch-state-record-error (agent-switch-read-state))))
      (signal 'agent-switch-validation-error
              (list (concat "state.json is damaged; reset it before writing: "
                            state-error))))
    (agent-switch--delete-profile-file-only profile)
    (condition-case error-value
        (agent-switch-state-remove-profile
         client-id (agent-switch-profile-id profile))
      (error
       (setf (agent-switch-file-state-hash snapshot) :missing)
       (condition-case rollback-error
           (agent-switch-restore-file snapshot)
         (error
          (signal 'agent-switch-error
                  (list (format "Profile deletion failed; restore also failed: %s"
                                (agent-switch--safe-error-message
                                 rollback-error))))))
       (signal (car error-value) (cdr error-value))))
    profile))

(defun agent-switch-diagnostics-data ()
  "Return sanitized diagnostics as a stable JSON-like object."
  (let ((data (make-hash-table :test #'equal)))
    (puthash "data_directory" (agent-switch--directory) data)
    (puthash "state_file" (agent-switch-state-path) data)
    (puthash "state_status"
             (or (agent-switch-state-record-error (agent-switch-read-state))
                 "ok")
             data)
    (puthash "registered_clients"
             (vconcat (mapcar #'agent-switch-client-id
                              (agent-switch-clients)))
             data)
    (dolist (library '("toml" "tomelr"))
      (puthash (concat library "_available")
               (and (locate-library library) t) data))
    data))

(provide 'agent-switch-operations)

;;; agent-switch-operations.el ends here
