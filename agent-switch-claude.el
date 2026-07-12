;;; agent-switch-claude.el --- Claude Code adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Built-in Claude Code adapter.

;;; Code:

(require 'agent-switch-adapter-utils)

(defcustom agent-switch-claude-config-directory
  (expand-file-name "~/.claude/")
  "Claude Code configuration directory."
  :type 'directory
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

(defun agent-switch--claude-secret-location-comments ()
  "Return likely pre-adoption Claude secret locations."
  (list
   (format "Password may currently be in %s under env.ANTHROPIC_API_KEY or env.ANTHROPIC_AUTH_TOKEN."
           (abbreviate-file-name (agent-switch--claude-settings-path)))
   "It may instead come from ANTHROPIC_API_KEY, ANTHROPIC_AUTH_TOKEN, or a Claude Code apiKeyHelper."))

(defun agent-switch--claude-capture-current (client current _context)
  "Capture CLIENT CURRENT state with Claude-specific secret hints."
  (agent-switch--capture-current-with-comments
   client current (agent-switch--claude-secret-location-comments)))

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

(defun agent-switch--claude-profile-template (_client _context)
  "Return a new Claude Profile payload template."
  (agent-switch--template-object
   (cons "env" (agent-switch--template-object
                '("ANTHROPIC_BASE_URL" . "")
                '("ANTHROPIC_MODEL" . "")
                '("ANTHROPIC_DEFAULT_HAIKU_MODEL" . "")
                '("ANTHROPIC_DEFAULT_SONNET_MODEL" . "")
                '("ANTHROPIC_DEFAULT_OPUS_MODEL" . "")))))

(defun agent-switch-register-claude ()
  "Register the built-in Claude Code adapter and client."
  (agent-switch-define-adapter claude
    :name "Claude Code"
    :current #'agent-switch--claude-current
    :activate #'agent-switch--claude-activate
    :validate #'agent-switch--claude-validate
    :snapshot #'agent-switch--claude-snapshot
    :rollback #'agent-switch--rollback-files
    :profile-current-p #'agent-switch--claude-profile-current-p
    :capture-current #'agent-switch--claude-capture-current
    :watch-paths #'agent-switch--claude-watch-paths
    :profile-template #'agent-switch--claude-profile-template
    :profile-columns #'agent-switch--claude-profile-columns)
  (agent-switch-register-client 'claude :name "Claude Code" :adapter 'claude))

(provide 'agent-switch-claude)

;;; agent-switch-claude.el ends here
