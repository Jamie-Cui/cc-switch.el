;;; agent-switch-adapter-utils.el --- Shared adapter mechanisms -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shared file, secret-reference, capture, and matching mechanisms used by
;; built-in agent-switch adapters.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'agent-switch-core)
(require 'agent-switch-storage)

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

(defun agent-switch--auth-source-reference
    (machine login &optional delivery comments)
  "Return an auth-source reference for MACHINE, LOGIN, DELIVERY, and COMMENTS."
  (let ((reference (make-hash-table :test #'equal))
        (authinfo (make-hash-table :test #'equal)))
    (puthash "source" "auth-source" reference)
    (puthash "machine" machine authinfo)
    (puthash "login" login authinfo)
    (when comments
      (puthash "comments" (vconcat comments) authinfo))
    (puthash "authinfo" authinfo reference)
    (when delivery (puthash "delivery" delivery reference))
    reference))

(defun agent-switch--captured-secret-reference (machine path comments)
  "Return an auth-source reference for MACHINE, secret PATH, and COMMENTS."
  (agent-switch--auth-source-reference
   machine (if path (string-join path ".") "secret") nil comments))

(defun agent-switch--capture-secret-safe-value
    (value machine comments &optional path)
  "Copy VALUE, replacing secret markers with MACHINE references and COMMENTS."
  (cond
   ((agent-switch--secret-marker-p value)
    (agent-switch--captured-secret-reference machine path comments))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test #'equal)))
      (maphash
       (lambda (key child)
         (puthash
          key
          (agent-switch--capture-secret-safe-value
           child machine comments (append path (list (format "%s" key))))
          copy))
       value)
      copy))
   ((vectorp value)
    (let ((copy (make-vector (length value) nil)))
      (dotimes (index (length value))
        (aset copy index
              (agent-switch--capture-secret-safe-value
               (aref value index) machine comments
               (append path (list (number-to-string index))))))
      copy))
   ((consp value)
    (cl-loop for child in value
             for index from 0
             collect (agent-switch--capture-secret-safe-value
                      child machine comments
                      (append path (list (number-to-string index))))))
   (t value)))

(defun agent-switch--capture-current-with-comments
    (client current comments)
  "Capture CLIENT CURRENT state with generated references carrying COMMENTS."
  (let ((machine (agent-switch--capture-authinfo-machine client current)))
    (agent-switch-capture-result-create
     :payload (agent-switch--capture-secret-safe-value
               current machine comments)
     :complete-p t
     :warnings nil)))

(defun agent-switch--capture-current (client current _context)
  "Capture CLIENT CURRENT state with generated auth-source references."
  (agent-switch--capture-current-with-comments client current nil))

(defun agent-switch--secret-reference-operational-copy (reference)
  "Copy REFERENCE without non-operational authinfo comments."
  (let* ((copy (agent-switch-json-copy reference))
         (authinfo (gethash "authinfo" copy)))
    (when (hash-table-p authinfo)
      (remhash "comments" authinfo))
    copy))

(defun agent-switch--json-subset-p (expected actual)
  "Return non-nil when JSON EXPECTED is represented by ACTUAL.
Secret references match any configured secret; resolved strings match hashed
secret markers exactly."
  (cond
   ((agent-switch-secret-reference-p expected)
    (or (agent-switch--secret-marker-p actual)
        (and (agent-switch-secret-reference-p actual)
             (agent-switch--json-value-equal-p
              (agent-switch--secret-reference-operational-copy expected)
              (agent-switch--secret-reference-operational-copy actual)))))
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

(defun agent-switch--template-object (&rest entries)
  "Return a JSON object populated from ENTRIES cons cells."
  (let ((object (make-hash-table :test #'equal)))
    (dolist (entry entries object)
      (puthash (car entry) (cdr entry) object))))

(provide 'agent-switch-adapter-utils)

;;; agent-switch-adapter-utils.el ends here
