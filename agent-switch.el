;;; agent-switch.el --- Manage LLM agent provider profiles -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Jamie
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "29.1") (transient "0.4") (toml "1.0.0") (tomelr "0.4.3"))
;; Version: 0.1.0
;; URL: https://github.com/Jamie-Cui/agent-switch.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; agent-switch is an extensible dashboard for selecting provider profiles
;; used by LLM agent clients.  Built-in adapters support Claude Code, Codex,
;; and OpenCode global configuration.  Third-party clients,
;; adapters, and profiles can be registered entirely from Emacs Lisp.

;;; Code:

(require 'agent-switch-core)
(require 'agent-switch-storage)
(require 'agent-switch-adapters)
(require 'agent-switch-operations)
(require 'agent-switch-ui)

;;;###autoload
(defun agent-switch ()
  "Open the agent-switch dashboard."
  (interactive)
  (agent-switch-dashboard))

(provide 'agent-switch)

;;; agent-switch.el ends here
