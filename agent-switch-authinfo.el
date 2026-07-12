;;; agent-switch-authinfo.el --- Authinfo token helper -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A narrow batch entry point for clients that support command-backed
;; authentication.  Primary output is the token on stdout; diagnostics go to
;; stderr and failure is reported through the process exit status.

;;; Code:

(require 'auth-source)
(require 'subr-x)

(defun agent-switch-authinfo--secret (source machine login)
  "Return the secret from authinfo SOURCE matching MACHINE and LOGIN."
  (let* ((auth-sources (list (expand-file-name source)))
         (auth-source-do-cache nil)
         (match (car (auth-source-search
                      :host machine :user login :max 1
                      :require '(:secret))))
         (secret (plist-get match :secret))
         (value (if (functionp secret) (funcall secret) secret)))
    (and (stringp value) (not (string-empty-p value)) value)))

(defun agent-switch-authinfo-run (arguments output error-output)
  "Resolve ARGUMENTS, writing token to OUTPUT or diagnostics to ERROR-OUTPUT.
ARGUMENTS must be authinfo file, machine, and login.  Return a process-style
status code without exposing secret values in diagnostics."
  (pcase arguments
    (`(,source ,machine ,login)
     (if (and (stringp source) (file-readable-p (expand-file-name source))
              (stringp machine) (not (string-empty-p machine))
              (stringp login) (not (string-empty-p login)))
         (condition-case nil
             (if-let* ((secret
                        (agent-switch-authinfo--secret source machine login)))
                 (progn (princ secret output) 0)
               (princ (format "No authinfo secret found for %s/%s\n"
                              machine login)
                      error-output)
               1)
           (error
            (princ (format "Authinfo lookup failed for %s/%s\n"
                           machine login)
                   error-output)
            1))
       (princ "Usage: agent-switch-authinfo SOURCE MACHINE LOGIN\n"
              error-output)
       2))
    (_
     (princ "Usage: agent-switch-authinfo SOURCE MACHINE LOGIN\n"
            error-output)
     2)))

(defun agent-switch-authinfo-main ()
  "Run the authinfo helper using remaining command-line arguments."
  (let ((arguments command-line-args-left))
    (while (equal (car arguments) "--")
      (setq arguments (cdr arguments)))
    (kill-emacs
     (agent-switch-authinfo-run
      arguments standard-output #'external-debugging-output))))

(provide 'agent-switch-authinfo)

;;; agent-switch-authinfo.el ends here
