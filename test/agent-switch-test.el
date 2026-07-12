;;; agent-switch-test.el --- Tests for agent-switch.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'agent-switch)
(require 'agent-switch-authinfo)

(defmacro agent-switch-test--with-root (root &rest body)
  "Run BODY with isolated registries and configuration below ROOT."
  (declare (indent 1))
  `(let* ((,root (make-temp-file "agent-switch-test-" t))
          (agent-switch-directory (expand-file-name "data/" ,root))
          (agent-switch-authinfo-file (expand-file-name ".authinfo" ,root))
          (agent-switch-claude-config-directory
           (expand-file-name ".claude/" ,root))
          (agent-switch-codex-home (expand-file-name ".codex/" ,root))
          (agent-switch-opencode-config-file
           (expand-file-name "opencode/opencode.jsonc" ,root))
          (agent-switch-confirm-canonical-rewrite nil)
          (agent-switch--adapters (make-hash-table :test #'equal))
          (agent-switch--clients (make-hash-table :test #'equal))
          (agent-switch--client-order nil)
          (agent-switch--external-profiles (make-hash-table :test #'equal))
          (agent-switch--discovery-cache (make-hash-table :test #'equal))
          (agent-switch--running-jobs (make-hash-table :test #'equal)))
     (unwind-protect
         (progn
           (agent-switch-register-builtins)
           ,@body)
       (ignore-errors (delete-directory ,root t)))))

(defun agent-switch-test--hash (&rest pairs)
  "Return an equal-tested hash table from PAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(defun agent-switch-test--secret-reference (machine login &optional delivery)
  "Return an auth-source reference for MACHINE, LOGIN, and optional DELIVERY."
  (let ((reference
         (agent-switch-test--hash
          "source" "auth-source"
          "authinfo" (agent-switch-test--hash
                      "machine" machine
                      "login" login))))
    (when delivery (puthash "delivery" delivery reference))
    reference))

(defun agent-switch-test--profile (client-id id payload &optional name)
  "Return a new managed Profile for CLIENT-ID, ID, and PAYLOAD."
  (let* ((client (agent-switch-get-client client-id t))
         (adapter (and client
                       (agent-switch-get-adapter
                        (agent-switch-client-adapter-id client) t))))
    (agent-switch--make-profile
     :id id :client-id client-id :name (or name id)
     :payload payload :ownership 'managed :valid-p t
     :payload-version (and adapter
                           (agent-switch-adapter-payload-version adapter)))))

(defun agent-switch-test--run-job (job)
  "Run synchronous JOB and return its value or signal its error."
  (let (settled value error-value)
    (agent-switch-job-start
     job
     (lambda (result) (setq settled t value result))
     (lambda (error-result) (setq settled t error-value error-result)))
    (unless settled
      (error "Test Job did not settle synchronously"))
    (when error-value
      (signal (car error-value) (cdr error-value)))
    value))

(defun agent-switch-test--read-json-file (path)
  "Read JSON object from PATH."
  (agent-switch-parse-json
   (with-temp-buffer
     (insert-file-contents path)
   (buffer-string))))

(ert-deftest agent-switch-authinfo-helper-has-unix-output-contract ()
  (let ((source (make-temp-file "agent-switch-authinfo-"))
        (output (generate-new-buffer " *agent-switch-authinfo-out*"))
        (errors (generate-new-buffer " *agent-switch-authinfo-err*")))
    (unwind-protect
        (progn
          (write-region
           "machine relay.example.com login codex.api-key password test-secret\n"
           nil source)
          (should (= 0 (agent-switch-authinfo-run
                        (list source "relay.example.com" "codex.api-key")
                        output errors)))
          (with-current-buffer output
            (should (equal (buffer-string) "test-secret")))
          (with-current-buffer errors
            (should (string-empty-p (buffer-string))))
          (with-current-buffer output (erase-buffer))
          (should (= 1 (agent-switch-authinfo-run
                        (list source "missing.example.com" "codex.api-key")
                        output errors)))
          (with-current-buffer output
            (should (string-empty-p (buffer-string))))
          (with-current-buffer errors
            (should (string-match-p "missing.example.com" (buffer-string)))
            (should-not (string-match-p "test-secret" (buffer-string)))))
      (ignore-errors (delete-file source))
      (kill-buffer output)
      (kill-buffer errors))))

(ert-deftest agent-switch-identifiers-reject-path-traversal ()
  (should-error (agent-switch--string-id "../escape" "profile")
                :type 'agent-switch-validation-error)
  (should (equal (agent-switch--string-id 'openai-main "profile")
                 "openai-main")))

(ert-deftest agent-switch-registry-supports-elisp-extensions ()
  (agent-switch-test--with-root root
    (agent-switch-define-adapter demo
      :name "Demo"
      :current (lambda (_client _context)
                 (agent-switch-test--hash "model" "one"))
      :activate (lambda (_client _profile _context) t))
    (agent-switch-register-client 'demo-client :adapter 'demo :name "Demo Client")
    (agent-switch-register-profile
     'demo-client 'from-elisp :name "From Elisp"
     :payload (agent-switch-test--hash "model" "one"))
    (let ((profile (car (agent-switch-external-profiles "demo-client"))))
      (should (eq (agent-switch-profile-ownership profile) 'external))
      (should (equal (agent-switch-profile-id profile) "from-elisp")))))

(ert-deftest agent-switch-adapter-requires-current-and-activate ()
  (agent-switch-test--with-root root
    (should-error
     (agent-switch-register-adapter 'broken :current #'ignore)
     :type 'agent-switch-validation-error)
    (should-error
     (agent-switch-register-adapter
      'unsupported
      :current #'ignore :activate #'ignore :status #'ignore)
     :type 'agent-switch-validation-error)
    (should-error
     (agent-switch-register-client
      'unsupported-client :adapter 'claude :metadata 'unused)
     :type 'agent-switch-validation-error)
    (should-error
     (agent-switch-register-profile
      'claude 'unsupported-profile :metadata 'unused)
     :type 'agent-switch-validation-error)))

(ert-deftest agent-switch-profile-roundtrip-uses-versioned-json ()
  (agent-switch-test--with-root root
    (let* ((profile (agent-switch-test--profile
                     "claude" "work"
                     (agent-switch-test--hash
                      "env" (agent-switch-test--hash
                             "ANTHROPIC_BASE_URL" "https://example.test"))
                     "Work"))
           (saved (agent-switch-save-profile profile))
           (loaded (car (agent-switch-load-managed-profiles "claude")))
           (json (agent-switch-test--read-json-file
                  (agent-switch-profile-source saved))))
      (should (equal (gethash "schema_version" json) 1))
      (should (equal (agent-switch-profile-id loaded) "work"))
      (should (eq (agent-switch-profile-ownership loaded) 'managed))
      (should (stringp (agent-switch-profile-source-hash loaded))))))

(ert-deftest agent-switch-plaintext-secrets-are-rejected ()
  (agent-switch-test--with-root root
    (let ((profile (agent-switch-test--profile
                    "claude" "unsafe"
                    (agent-switch-test--hash
                     "env" (agent-switch-test--hash
                            "ANTHROPIC_AUTH_TOKEN" "secret-value")))))
      (should-error (agent-switch-save-profile profile)
                    :type 'agent-switch-validation-error))))

(ert-deftest agent-switch-external-plaintext-secrets-are-rejected ()
  (agent-switch-test--with-root root
    (should-error
     (agent-switch-register-profile
      'claude 'unsafe
      :payload (agent-switch-test--hash
                "env" (agent-switch-test--hash
                       "ANTHROPIC_AUTH_TOKEN" "plain-secret")))
     :type 'agent-switch-validation-error)))

(ert-deftest agent-switch-activation-revalidates-untrusted-profile-payload ()
  (agent-switch-test--with-root root
    (let ((activated nil)
          (profile (agent-switch--make-profile
                    :id "unsafe" :client-id "unsafe-client" :name "Unsafe"
                    :payload (agent-switch-test--hash "api_token" "plain-secret")
                    :ownership 'external :source 'adapter :valid-p t)))
      (agent-switch-define-adapter unsafe-adapter
        :current (lambda (_client _context) nil)
        :activate (lambda (_client _profile _context)
                    (setq activated t)))
      (let ((client (agent-switch-register-client
                     'unsafe-client :adapter 'unsafe-adapter)))
        (should-error (agent-switch-activation-job client profile)
                      :type 'agent-switch-validation-error)
        (should-not activated)
        (should-not (file-exists-p (agent-switch-state-path)))))))

(ert-deftest agent-switch-secret-references-resolve-without-persistence ()
  (agent-switch-test--with-root root
    (let* ((reference (agent-switch-test--secret-reference
                       "agent-switch.test" "test"))
           (profile (agent-switch-test--profile
                     "claude" "safe"
                     (agent-switch-test--hash
                      "env" (agent-switch-test--hash
                             "ANTHROPIC_AUTH_TOKEN" reference)))))
      (cl-letf (((symbol-function 'auth-source-search)
                 (lambda (&rest _arguments)
                   (should-not auth-source-do-cache)
                   (list (list :secret (lambda () "resolved-secret"))))))
        (agent-switch-save-profile profile)
        (let* ((result (agent-switch-resolve-profile-secrets profile))
               (resolved (car result)))
          (should (equal
                   (gethash
                    "ANTHROPIC_AUTH_TOKEN"
                    (gethash "env" (agent-switch-profile-payload resolved)))
                   "resolved-secret"))
          (should-not
           (string-match-p
            "resolved-secret"
            (agent-switch--read-file-text
             (agent-switch-profile-source profile)))))))))

(ert-deftest agent-switch-command-delivered-secret-stays-a-reference ()
  (agent-switch-test--with-root root
    (let* ((reference (agent-switch-test--secret-reference
                       "relay.example.com" "codex.api-key" "command"))
           (profile (agent-switch-test--profile
                     "codex" "deferred"
                     (agent-switch-test--hash "credential" reference)))
           secret-called)
      (cl-letf (((symbol-function 'auth-source-search)
                 (lambda (&rest _arguments)
                   (list (list :secret
                               (lambda ()
                                 (setq secret-called t)
                                 "must-not-resolve"))))))
        (let* ((result (agent-switch-resolve-profile-secrets profile))
               (resolved (car result)))
          (should (agent-switch--json-value-equal-p
                   (gethash "credential"
                            (agent-switch-profile-payload resolved))
                   reference))
          (should-not (cdr result))
          (should-not secret-called)))
      (cl-letf (((symbol-function 'auth-source-search)
                 (lambda (&rest _arguments) nil)))
        (should-error (agent-switch-resolve-profile-secrets profile)
                      :type 'agent-switch-error)))))

(ert-deftest agent-switch-environment-secret-references-are-rejected ()
  (agent-switch-test--with-root root
    (let ((profile
           (agent-switch-test--profile
            "claude" "env-secret"
            (agent-switch-test--hash
             "env" (agent-switch-test--hash
                    "ANTHROPIC_AUTH_TOKEN"
                    (agent-switch-test--hash
                     "source" "env" "name" "ANTHROPIC_AUTH_TOKEN"))))))
      (should-error (agent-switch-save-profile profile)
                    :type 'agent-switch-validation-error))))

(ert-deftest agent-switch-auth-source-reference-resolves-function-secret ()
  (let ((reference (agent-switch-test--secret-reference
                    "api.example.test" "agent")))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _arguments)
                 (should-not auth-source-do-cache)
                 (list (list :secret (lambda () "auth-secret"))))))
      (should (equal (agent-switch--resolve-secret-reference reference)
                     "auth-secret")))))

(ert-deftest agent-switch-corrupt-profile-is-isolated ()
  (agent-switch-test--with-root root
    (let ((directory (agent-switch-profiles-directory "claude")))
      (make-directory directory t)
      (write-region "{broken" nil (expand-file-name "broken.json" directory))
      (agent-switch-save-profile
       (agent-switch-test--profile
        "claude" "valid"
        (agent-switch-test--hash
         "env" (agent-switch-test--hash
                "ANTHROPIC_BASE_URL" "https://example.test"))))
      (let ((profiles (agent-switch-load-managed-profiles "claude")))
        (should (= (length profiles) 2))
        (should-not (agent-switch-profile-valid-p
                     (cl-find "broken" profiles
                              :key #'agent-switch-profile-id :test #'equal)))
        (should (agent-switch-profile-valid-p
                 (cl-find "valid" profiles
                          :key #'agent-switch-profile-id :test #'equal)))))))

(ert-deftest agent-switch-damaged-state-is-read-only-until-reset ()
  (agent-switch-test--with-root root
    (make-directory (agent-switch--directory) t)
    (write-region "not-json" nil (agent-switch-state-path))
    (should (agent-switch-state-record-error (agent-switch-read-state)))
    (should-error (agent-switch-state-set-last-selected "claude" "work")
                  :type 'agent-switch-validation-error)
    (let ((backup (agent-switch-reset-state)))
      (should (file-exists-p backup))
      (should-not (agent-switch-state-record-error (agent-switch-read-state))))))

(ert-deftest agent-switch-state-v1-migrates-to-selection-records ()
  (agent-switch-test--with-root root
    (let* ((state (agent-switch-test--hash
                   "schema_version" 1
                   "last_selected" (agent-switch-test--hash "claude" "work")
                   "applied_profiles"
                   (agent-switch-test--hash
                    "claude" (agent-switch-test--hash
                              "payload" (agent-switch-test--hash "model" "old")))
                   "unprotected_confirmed" []
                   "canonical_confirmations" (agent-switch-test--hash)))
           (path (agent-switch-state-path)))
      (make-directory (file-name-directory path) t)
      (write-region (agent-switch-json-serialize state) nil path)
      (let ((selection (agent-switch-state-selection "claude")))
        (should (equal (gethash "profile_id" selection) "work"))
        (should (equal (gethash "source" selection) "applied"))
        (should (equal (gethash "model" (gethash "payload" selection)) "old"))
        (should (agent-switch-state-client-initialized-p "claude")))
      (agent-switch-update-state #'identity)
      (let ((written (agent-switch-test--read-json-file path)))
        (should (equal (gethash "schema_version" written) 2))
        (should (hash-table-p (gethash "selections" written)))
        (should-not (gethash "last_selected" written))
        (should-not (gethash "applied_profiles" written))))))

(ert-deftest agent-switch-optimistic-write-preserves-external-change ()
  (agent-switch-test--with-root root
    (let* ((path (expand-file-name "conflict.json" root))
           (snapshot (agent-switch-capture-file path)))
      (write-region "external" nil path)
      (should-error
       (agent-switch-write-text-atomic
        path "ours" (agent-switch-file-state-hash snapshot))
       :type 'agent-switch-conflict)
      (should (equal (agent-switch--read-file-text path) "external")))))

(ert-deftest agent-switch-rollback-preserves-post-apply-external-change ()
  (agent-switch-test--with-root root
    (let ((path (expand-file-name "rollback.json" root)))
      (write-region "initial" nil path nil 'silent)
      (let* ((snapshot (agent-switch-capture-file path))
             (context (list :snapshot (list snapshot))))
        (agent-switch--write-live-text path "applied" context)
        (write-region "external" nil path nil 'silent)
        (should-error (agent-switch-restore-file snapshot)
                      :type 'agent-switch-conflict)
        (should (equal (agent-switch--read-file-text path) "external"))))))

(ert-deftest agent-switch-rollback-preserves-externally-replaced-created-file ()
  (agent-switch-test--with-root root
    (let* ((path (expand-file-name "created.json" root))
           (snapshot (agent-switch-capture-file path))
           (context (list :snapshot (list snapshot))))
      (agent-switch--write-live-text path "applied" context)
      (write-region "external" nil path nil 'silent)
      (should-error (agent-switch-restore-file snapshot)
                    :type 'agent-switch-conflict)
      (should (equal (agent-switch--read-file-text path) "external")))))

(ert-deftest agent-switch-profiles-are-ordered-by-id ()
  (agent-switch-test--with-root root
    (dolist (id '("a" "b"))
      (agent-switch-save-profile
       (agent-switch-test--profile
        "claude" id
        (agent-switch-test--hash
         "env" (agent-switch-test--hash
                "ANTHROPIC_BASE_URL" (concat "https://" id))))))
    (agent-switch-register-profile
     'claude 'external :name "External"
     :payload (agent-switch-test--hash
               "env" (agent-switch-test--hash
                      "ANTHROPIC_BASE_URL" "https://external")))
    (should (equal (mapcar #'agent-switch-profile-id
                           (agent-switch-profiles "claude"))
                   '("a" "b" "external")))))

(ert-deftest agent-switch-duplicate-profile-identities-become-one-conflict ()
  (agent-switch-test--with-root root
    (agent-switch-save-profile
     (agent-switch-test--profile
      "claude" "duplicate"
      (agent-switch-test--hash
       "env" (agent-switch-test--hash
              "ANTHROPIC_BASE_URL" "https://managed.test"))))
    (agent-switch-register-profile
     'claude 'duplicate :name "External"
     :payload (agent-switch-test--hash
               "env" (agent-switch-test--hash
                      "ANTHROPIC_BASE_URL" "https://external.test")))
    (let ((profiles (agent-switch-profiles "claude")))
      (should (= (length profiles) 1))
      (should (equal (agent-switch-profile-id (car profiles)) "duplicate"))
      (should-not (agent-switch-profile-valid-p (car profiles)))
      (should (string-match-p "Duplicate Profile ID"
                              (agent-switch-profile-error (car profiles)))))))

(ert-deftest agent-switch-discovered-plaintext-profile-is-isolated ()
  (agent-switch-test--with-root root
    (let ((profile (agent-switch--make-profile
                    :id "unsafe" :client-id "discovered-client" :name "Unsafe"
                    :payload (agent-switch-test--hash "api_token" "plain-secret")
                    :ownership 'external :source 'adapter :valid-p t)))
      (agent-switch-define-adapter discovered-unsafe-adapter
        :current (lambda (_client _context) nil)
        :activate (lambda (_client _profile _context) t)
        :discover (lambda (_client _context) (list profile)))
      (agent-switch-register-client
       'discovered-client :adapter 'discovered-unsafe-adapter)
      (let ((loaded (car (agent-switch-profiles "discovered-client"))))
        (should loaded)
        (should-not (agent-switch-profile-valid-p loaded))
        (should (string-match-p "Plaintext secret"
                                (agent-switch-profile-error loaded)))))))

(ert-deftest agent-switch-activation-rolls-back-on-verification-failure ()
  (agent-switch-test--with-root root
    (let ((state (agent-switch-test--hash "value" "old")))
      (agent-switch-define-adapter rollback-demo
        :current (lambda (_client _context) state)
        :activate (lambda (_client _profile _context)
                    (setq state (agent-switch-test--hash "value" "wrong")))
        :snapshot (lambda (_client _profile _context)
                    (agent-switch-json-copy state))
        :rollback (lambda (_client snapshot _context)
                    (setq state snapshot)))
      (let* ((client (agent-switch-register-client
                      'rollback-demo :adapter 'rollback-demo))
             (profile (agent-switch-test--profile
                       "rollback-demo" "new"
                       (agent-switch-test--hash "value" "expected"))))
        (should-error
         (agent-switch-test--run-job
          (agent-switch-activation-job client profile))
         :type 'agent-switch-error)
        (should (equal (gethash "value" state) "old"))))))

(ert-deftest agent-switch-async-discovery-is-supported ()
  (agent-switch-test--with-root root
    (let ((discovered
           (agent-switch--make-profile
            :id "remote" :client-id "async-client" :name "Remote"
            :payload (agent-switch-test--hash "value" "one")
            :ownership 'external :source 'adapter :valid-p t)))
      (agent-switch-define-adapter async-adapter
        :current (lambda (_client _context)
                   (agent-switch-test--hash "value" "one"))
        :activate (lambda (_client _profile _context) t)
        :discover
        (lambda (_client _context)
          (agent-switch-job-create
           :starter (lambda (resolve _reject)
                      (funcall resolve (list discovered))))))
      (agent-switch-register-client 'async-client :adapter 'async-adapter)
      (should-not (agent-switch-profiles "async-client"))
      (should (equal (mapcar #'agent-switch-profile-id
                             (agent-switch-profiles "async-client"))
                     '("remote"))))))

(ert-deftest agent-switch-dashboard-initializes-live-config-only-once ()
  (agent-switch-test--with-root root
    (let ((captures 0))
      (agent-switch-define-adapter bootstrap-dashboard-adapter
        :current (lambda (_client _context)
                   (agent-switch-test--hash "model" "live"))
        :activate (lambda (_client _profile _context) t)
        :capture-current
        (lambda (_client current _context)
          (setq captures (1+ captures))
          (agent-switch-json-copy current)))
      (let ((client (agent-switch-register-client
                     'bootstrap-dashboard-client
                     :adapter 'bootstrap-dashboard-adapter)))
        (cl-letf (((symbol-function 'agent-switch--random-profile-id)
                   (lambda (_client-id) "p-bootstrap")))
          (with-temp-buffer
            (agent-switch-mode)
            (let* ((view (agent-switch--client-view client))
                   (profile (car (agent-switch-client-view-profiles view))))
              (should profile)
              (should (equal (agent-switch-profile-id profile) "p-bootstrap"))
              (should (equal (agent-switch-profile-name profile) "default"))
              (should (equal (agent-switch-state-last-selected
                              "bootstrap-dashboard-client")
                             "p-bootstrap"))
              (should (= captures 1))
              (agent-switch-delete-managed-profile profile)
              (should-not (agent-switch-client-view-profiles
                           (agent-switch--client-view client)))
              (should (= captures 1)))))))))

(ert-deftest agent-switch-dashboard-initializes-incomplete-live-config ()
  (agent-switch-test--with-root root
    (let ((current (agent-switch-test--hash "model" "live")))
      (agent-switch-define-adapter incomplete-bootstrap-adapter
        :current (lambda (_client _context) current)
        :activate (lambda (_client _profile _context) t)
        :capture-current
        (lambda (_client _current _context)
          (agent-switch-capture-result-create
           :payload (agent-switch-test--hash "model" "live")
           :complete-p nil
           :warnings '("auth-source reference required at token"))))
      (let ((client (agent-switch-register-client
                     'incomplete-bootstrap-client
                     :adapter 'incomplete-bootstrap-adapter)))
        (with-temp-buffer
          (agent-switch-mode)
          (let* ((view (agent-switch--client-view client))
                 (profile (car (agent-switch-client-view-profiles view))))
            (should profile)
            (should (agent-switch-profile-setup-required-p profile))
            (should-not (agent-switch-state-last-selected
                         "incomplete-bootstrap-client"))
            (should (agent-switch-state-client-initialized-p
                     "incomplete-bootstrap-client"))))))))

(ert-deftest agent-switch-claude-patches-owned-env-only ()
  (agent-switch-test--with-root root
    (make-directory agent-switch-claude-config-directory t)
    (let* ((path (agent-switch--claude-settings-path))
           (settings
            (agent-switch-test--hash
             "permissions" (agent-switch-test--hash "allow" ["Read"])
             "env" (agent-switch-test--hash
                    "OTHER" "keep"
                    "ANTHROPIC_MODEL" "old")))
           (reference (agent-switch-test--secret-reference
                       "claude-relay.test" "agent"))
           (payload (agent-switch-test--hash
                     "env" (agent-switch-test--hash
                            "ANTHROPIC_BASE_URL" "https://relay.test"
                            "ANTHROPIC_AUTH_TOKEN" reference
                            "ANTHROPIC_MODEL" "new")))
           (profile (agent-switch-test--profile
                     "claude" "relay" payload "Relay"))
           (client (agent-switch-get-client "claude")))
      (write-region (agent-switch-json-serialize settings) nil path)
      (cl-letf (((symbol-function 'auth-source-search)
                 (lambda (&rest _arguments)
                   (list (list :secret (lambda () "live-secret"))))))
        (agent-switch-test--run-job
         (agent-switch-activation-job client profile))
        (let* ((written (agent-switch-test--read-json-file path))
               (env (gethash "env" written)))
          (should (hash-table-p (gethash "permissions" written)))
          (should (equal (gethash "OTHER" env) "keep"))
          (should (equal (gethash "ANTHROPIC_MODEL" env) "new"))
          (should (equal (gethash "ANTHROPIC_AUTH_TOKEN" env)
                         "live-secret"))
          (should (cl-some
                   (lambda (file)
                     (string-match-p "agent-switch\\.bak" file))
                   (directory-files agent-switch-claude-config-directory))))
        (should-not
         (string-match-p
          "live-secret"
          (format "%S" (agent-switch--claude-current client nil))))))))

(ert-deftest agent-switch-codex-structurally-preserves-unowned-settings ()
  (agent-switch-test--with-root root
    (make-directory agent-switch-codex-home t)
    (let* ((path (agent-switch--codex-config-path))
           (old (concat
                 "model = \"old/model\"\n"
                 "model_provider = \"old\"\n"
                 "sandbox_mode = \"workspace-write\"\n"
                 "developer_instructions = \"Preserve 中文 text\"\n\n"
                 "[model_providers.old]\n"
                 "base_url = \"https://old.test\"\n\n"
                 "[mcp_servers.demo]\n"
                 "command = \"demo\"\n"))
           (provider (agent-switch-test--hash
                      "base_url" "https://new.test"))
           (credential (agent-switch-test--secret-reference
                        "new.test" "codex.new.api-key" "command"))
           (payload (agent-switch-test--hash
                     "provider-id" "new"
                     "model" "new/model"
                     "small-model" "new/small"
                     "provider" provider
                     "credential" credential))
           (profile (agent-switch-test--profile "codex" "new" payload))
           (client (agent-switch-get-client "codex")))
      (write-region old nil path)
      (write-region
       "machine new.test login codex.new.api-key password test-secret\n"
       nil agent-switch-authinfo-file)
      (agent-switch-test--run-job
       (agent-switch-activation-job client profile))
      (let* ((config (agent-switch--read-toml-file path))
             (providers (agent-switch--alist-get "model_providers" config))
             (new-provider (agent-switch--alist-get "new" providers)))
        (should (equal (agent-switch--alist-get "sandbox_mode" config)
                       "workspace-write"))
        (should (equal (agent-switch--alist-get "developer_instructions" config)
                       "Preserve 中文 text"))
        (should (agent-switch--alist-get "mcp_servers" config))
        (should (agent-switch--alist-get "old" providers))
        (should (equal (agent-switch--alist-get "base_url" new-provider)
                       "https://new.test"))
        (should (agent-switch--toml-table-p
                 (agent-switch--alist-get "auth" new-provider)))
        (should-not (agent-switch--alist-get "env_key" new-provider))
        (should (equal (agent-switch--alist-get "model" config) "new/model"))))))

(ert-deftest agent-switch-codex-empty-provider-roundtrips-as-current ()
  (agent-switch-test--with-root root
    (make-directory agent-switch-codex-home t)
    (write-region
     (concat "model = \"llama3\"\n"
             "model_provider = \"ollama\"\n\n"
             "[model_providers.other]\n"
             "base_url = \"https://other.test\"\n")
     nil (agent-switch--codex-config-path))
    (let* ((client (agent-switch-get-client "codex"))
           (current (agent-switch--codex-current client nil))
           (provider (gethash "provider" current))
           (profile (agent-switch-test--profile
                     "codex" "default"
                     (agent-switch-json-copy current)
                     "Default")))
      (should (hash-table-p provider))
      (should (= (hash-table-count provider) 0))
      (agent-switch-save-profile profile)
      (let ((loaded (agent-switch-find-profile "codex" "default")))
        (should (agent-switch--codex-profile-current-p
                 client loaded current nil))
        (agent-switch-test--run-job
         (agent-switch-activation-job client loaded))))))

(ert-deftest agent-switch-codex-current-converts-env-key-to-authinfo-command ()
  (agent-switch-test--with-root root
    (make-directory agent-switch-codex-home t)
    (write-region
     (concat "model = \"relay/model\"\n"
             "model_provider = \"relay\"\n\n"
             "[model_providers.relay]\n"
             "base_url = \"https://relay.example.com/v1\"\n"
             "env_key = \"RELAY_API_KEY\"\n"
             "wire_api = \"responses\"\n")
     nil (agent-switch--codex-config-path))
    (let* ((client (agent-switch-get-client "codex"))
           (current (agent-switch--codex-current client nil))
           (provider (gethash "provider" current))
           (credential (gethash "credential" current))
           (authinfo (and (hash-table-p credential)
                          (gethash "authinfo" credential))))
      (should-not (gethash "env_key" provider))
      (should-not (gethash "auth" provider))
      (should (equal (gethash "source" credential) "auth-source"))
      (should (equal (gethash "delivery" credential) "command"))
      (should (equal (gethash "machine" authinfo) "relay.example.com"))
      (should (equal (gethash "login" authinfo) "codex.relay.api-key")))))

(ert-deftest agent-switch-codex-discovers-all-provider-tables ()
  (agent-switch-test--with-root root
    (make-directory agent-switch-codex-home t)
    (write-region
     (concat
      "model = \"gpt-test\"\n"
      "model_provider = \"openai\"\n\n"
      "[model_providers.sss]\n"
      "name = \"sss\"\n"
      "base_url = \"https://codex1.sssaicode.com/api/v1\"\n"
      "wire_api = \"responses\"\n"
      "env_key = \"SSS_API_KEY\"\n\n"
      "[model_providers.openrouter]\n"
      "name = \"openrouter\"\n"
      "base_url = \"https://openrouter.ai/api/v1\"\n"
      "env_key = \"OPENROUTER_API_KEY\"\n")
     nil (agent-switch--codex-config-path))
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "SSS_API_KEY" "must-not-be-captured")
      (let* ((profiles (agent-switch-profiles "codex"))
             (sss (cl-find "sss" profiles
                           :key #'agent-switch-profile-id :test #'equal))
             (openrouter
              (cl-find "openrouter" profiles
                       :key #'agent-switch-profile-id :test #'equal)))
        (should (equal (mapcar #'agent-switch-profile-id profiles)
                       '("openrouter" "sss")))
        (dolist (profile profiles)
          (should (eq (agent-switch-profile-ownership profile) 'external))
          (should (equal (agent-switch-profile-source profile)
                         (agent-switch--codex-config-path)))
          (should (equal (gethash "model"
                                  (agent-switch-profile-payload profile))
                         "gpt-test"))
          (should (agent-switch--profile-action-required-p profile)))
        (let* ((payload (agent-switch-profile-payload sss))
               (provider (gethash "provider" payload))
               (credential (gethash "credential" payload))
               (authinfo (gethash "authinfo" credential)))
          (should (equal (agent-switch-profile-name sss) "sss"))
          (should-not (gethash "env_key" provider))
          (should (equal (gethash "machine" authinfo)
                         "codex1.sssaicode.com"))
          (should (equal (gethash "login" authinfo)
                         "codex.sss.api-key"))
          (should-not (string-match-p
                       "must-not-be-captured" (format "%S" payload))))
        (let* ((credential
                (gethash "credential"
                         (agent-switch-profile-payload openrouter)))
               (authinfo (gethash "authinfo" credential)))
          (should (equal (gethash "machine" authinfo) "openrouter.ai"))
          (should (equal (gethash "login" authinfo)
                         "codex.openrouter.api-key")))
        (write-region
         (concat
          "machine codex1.sssaicode.com login codex.sss.api-key password one\n"
          "machine openrouter.ai login codex.openrouter.api-key password two\n")
         nil agent-switch-authinfo-file)
        (dolist (profile profiles)
          (should-not (agent-switch--profile-action-required-p profile)))))))

(ert-deftest agent-switch-codex-validation-requires-command-credential ()
  (agent-switch-test--with-root root
    (let* ((credential (agent-switch-test--secret-reference
                        "relay.example.com" "codex.relay.api-key" "command"))
           (payload (agent-switch-test--hash
                     "provider-id" "relay" "model" "relay/model"
                     "provider" (agent-switch-test--hash
                                 "base_url" "https://relay.example.com/v1")
                     "credential" credential))
           (profile (agent-switch-test--profile "codex" "relay" payload)))
      (should (agent-switch--codex-validate nil profile nil))
      (let ((missing (copy-agent-switch-profile profile)))
        (setf (agent-switch-profile-payload missing)
              (agent-switch-json-copy payload))
        (remhash "credential" (agent-switch-profile-payload missing))
        (should-error (agent-switch--codex-validate nil missing nil)
                      :type 'agent-switch-validation-error))
      (let* ((env-payload (agent-switch-json-copy payload))
             (env-profile (copy-agent-switch-profile profile)))
        (puthash "env_key" "RELAY_API_KEY" (gethash "provider" env-payload))
        (setf (agent-switch-profile-payload env-profile) env-payload)
        (should-error (agent-switch--codex-validate nil env-profile nil)
                      :type 'agent-switch-validation-error))
      (let ((local (agent-switch-test--profile
                    "codex" "local"
                    (agent-switch-test--hash
                     "provider-id" "ollama" "model" "llama3"
                     "provider" (agent-switch-test--hash)))))
        (should (agent-switch--codex-validate nil local nil))))))

(ert-deftest agent-switch-codex-apply-materializes-authinfo-command ()
  (agent-switch-test--with-root root
    (make-directory agent-switch-codex-home t)
    (write-region
     "model = \"old/model\"\nmodel_provider = \"old\"\n"
     nil (agent-switch--codex-config-path))
    (write-region
     "machine relay.example.com login codex.relay.api-key password test-secret\n"
     nil agent-switch-authinfo-file)
    (let* ((credential (agent-switch-test--secret-reference
                        "relay.example.com" "codex.relay.api-key" "command"))
           (payload (agent-switch-test--hash
                     "provider-id" "relay" "model" "relay/model"
                     "provider" (agent-switch-test--hash
                                 "base_url" "https://relay.example.com/v1"
                                 "wire_api" "responses")
                     "credential" credential))
           (profile (agent-switch-test--profile
                     "codex" "relay" payload "Relay"))
           (client (agent-switch-get-client "codex")))
      (agent-switch-test--run-job
       (agent-switch-activation-job client profile))
      (let* ((text (agent-switch--read-file-text
                    (agent-switch--codex-config-path)))
             (config (agent-switch--read-toml-file
                      (agent-switch--codex-config-path)))
             (provider (agent-switch--codex-provider-state config "relay"))
             (auth (agent-switch--alist-get "auth" provider))
             (command (agent-switch--alist-get "command" auth))
             (args (agent-switch--alist-get "args" auth))
             (current (agent-switch--codex-current client nil)))
        (should (agent-switch--toml-table-p auth))
        (should (stringp command))
        (should (vectorp args))
        (should (member (expand-file-name agent-switch-authinfo-file)
                        (append args nil)))
        (should (member "relay.example.com" (append args nil)))
        (should (member "codex.relay.api-key" (append args nil)))
        (should-not (agent-switch--alist-get "env_key" provider))
        (should-not (string-match-p "test-secret" text))
        (should (agent-switch--codex-profile-current-p
                 client profile current nil))
        (let ((output (generate-new-buffer " *codex-auth-helper*"))
              (error-file (make-temp-file "codex-auth-helper-error-")))
          (unwind-protect
              (let ((status (apply #'call-process command nil
                                   (list output error-file) nil
                                   (append args nil))))
                (unless (= status 0)
                  (ert-fail
                   (format "auth helper exited %s: %s"
                           status
                           (agent-switch--read-file-text error-file))))
                (with-current-buffer output
                  (should (equal (buffer-string) "test-secret")))
                (should (string-empty-p
                         (agent-switch--read-file-text error-file))))
            (kill-buffer output)
            (ignore-errors (delete-file error-file))))))))

(ert-deftest agent-switch-codex-native-openai-becomes-authinfo-managed ()
  (agent-switch-test--with-root root
    (make-directory agent-switch-codex-home t)
    (write-region
     "model = \"gpt-test\"\nmodel_provider = \"openai\"\n"
     nil (agent-switch--codex-config-path))
    (let* ((client (agent-switch-get-client "codex"))
           (current (agent-switch--codex-current client nil))
           (provider (gethash "provider" current))
           (credential (gethash "credential" current))
           (authinfo (and credential (gethash "authinfo" credential)))
           (profile (agent-switch-test--profile
                     "codex" "openai" (agent-switch-json-copy current))))
      (should (equal (gethash "provider-id" current) "openai"))
      (should (equal (gethash "base_url" provider)
                     "https://api.openai.com/v1"))
      (should (equal (gethash "delivery" credential) "command"))
      (should (equal (gethash "machine" authinfo) "api.openai.com"))
      (should (equal (gethash "login" authinfo) "codex.openai.api-key"))
      (let ((comments (append (gethash "comments" authinfo) nil)))
        (should (= (length comments) 2))
        (should (string-match-p "config\\.toml" (car comments)))
        (should (string-match-p "auth\\.json" (cadr comments))))
      (cl-letf (((symbol-function 'auth-source-search)
                 (lambda (&rest _arguments) nil)))
        (should (string-match-p
                 "(action required)"
                 (substring-no-properties
                  (agent-switch--profile-name-cell profile)))))
      (write-region
       "machine api.openai.com login codex.openai.api-key password test-secret\n"
       nil agent-switch-authinfo-file)
      (agent-switch-test--run-job
       (agent-switch-activation-job client profile))
      (let* ((config (agent-switch--read-toml-file
                      (agent-switch--codex-config-path)))
             (after (agent-switch--codex-current client nil)))
        (should (equal (agent-switch--alist-get "model_provider" config)
                       "agent-switch-openai"))
        (should (equal (gethash "provider-id" after) "openai"))
        (should (agent-switch--codex-profile-current-p
                 client profile after nil))))))

(ert-deftest agent-switch-current-match-respects-replaced-field-absence ()
  (agent-switch-test--with-root root
    (let* ((claude-profile
            (agent-switch-test--profile
             "claude" "without-token"
             (agent-switch-test--hash
              "env" (agent-switch-test--hash
                     "ANTHROPIC_BASE_URL" "https://example.test"))))
           (claude-current
            (agent-switch-test--hash
             "env" (agent-switch-test--hash
                    "ANTHROPIC_BASE_URL" "https://example.test"
                    "ANTHROPIC_AUTH_TOKEN"
                    (agent-switch--secret-marker "live-secret"))))
           (codex-profile
            (agent-switch-test--profile
             "codex" "without-small"
             (agent-switch-test--hash
              "provider-id" "relay" "model" "large"
              "provider" (agent-switch-test--hash))))
           (codex-current
            (agent-switch-test--hash
             "provider-id" "relay" "model" "large" "small-model" "small"
             "provider" (agent-switch-test--hash)))
           (opencode-profile
            (agent-switch-test--profile
             "opencode" "without-small"
             (agent-switch-test--hash
              "provider-id" "relay" "model" "relay/large"
              "provider" (agent-switch-test--hash))))
           (opencode-current
            (agent-switch-test--hash
             "provider-id" "relay" "model" "relay/large"
             "small-model" "relay/small"
             "provider" (agent-switch-test--hash))))
      (should-not (agent-switch--claude-profile-current-p
                   nil claude-profile claude-current nil))
      (should-not (agent-switch--codex-profile-current-p
                   nil codex-profile codex-current nil))
      (should-not (agent-switch--opencode-profile-current-p
                   nil opencode-profile opencode-current nil)))))

(ert-deftest agent-switch-opencode-jsonc-patch-preserves-other-config ()
  (agent-switch-test--with-root root
    (make-directory (file-name-directory agent-switch-opencode-config-file) t)
    (let* ((path (agent-switch--opencode-config-path))
           (project-file (expand-file-name "project/opencode.json" root))
           (payload (agent-switch-test--hash
                     "provider-id" "relay"
                     "model" "relay/large"
                     "small-model" "relay/small"
                     "provider" (agent-switch-test--hash
                                 "npm" "@ai-sdk/openai-compatible"
                                 "options" (agent-switch-test--hash
                                            "baseURL" "https://relay.test"))))
           (profile (agent-switch-test--profile
                     "opencode" "relay" payload))
           (client (agent-switch-get-client "opencode")))
      (write-region
       (concat "{\n  // keep semantics\n"
               "  \"model\": \"other/old\",\n"
               "  \"permission\": {\"bash\": \"ask\"},\n"
               "  \"provider\": {\"other\": {\"npm\": \"pkg\"}},\n}\n")
       nil path)
      (make-directory (file-name-directory project-file) t)
      (write-region "{\"model\":\"project/model\"}\n" nil project-file)
      (agent-switch-test--run-job
       (agent-switch-activation-job client profile))
      (let* ((written (agent-switch-test--read-json-file path))
             (providers (gethash "provider" written)))
        (should (equal (gethash "bash" (gethash "permission" written)) "ask"))
        (should (gethash "other" providers))
        (should (equal (gethash "model" written) "relay/large"))
        (should (equal (agent-switch--read-file-text project-file)
                       "{\"model\":\"project/model\"}\n"))))))

(ert-deftest agent-switch-dashboard-uses-internal-sections ()
  (agent-switch-test--with-root root
    (agent-switch-save-profile
     (agent-switch-test--profile
      "claude" "work"
      (agent-switch-test--hash
       "env" (agent-switch-test--hash
              "ANTHROPIC_BASE_URL" "https://example.test"))
      "Work"))
    (with-temp-buffer
      (agent-switch-mode)
      (agent-switch-refresh)
      (should (derived-mode-p 'special-mode))
      (should-not (derived-mode-p 'tabulated-list-mode))
      (should-not (gethash "status" agent-switch--sections))
      (goto-char (point-min))
      (should (looking-at "Data:"))
      (should-not (agent-switch--section-at-point t))
      (let* ((id "client/claude")
             (section (gethash id agent-switch--sections)))
        (should section)
        (should (agent-switch-section-expanded-p section))
        (goto-char (agent-switch-section-start section))
        (agent-switch-toggle-section)
        (setq section (gethash id agent-switch--sections))
        (should-not (agent-switch-section-expanded-p section))
        (should-not (gethash "client/claude/profile/work"
                             agent-switch--sections))
        (should (equal (agent-switch--point-section-id) id))))))

(ert-deftest agent-switch-dashboard-has-no-blank-lines-between-sections ()
  (agent-switch-test--with-root root
    (with-temp-buffer
      (agent-switch-mode)
      (agent-switch-refresh)
      (let ((clients
             (cl-remove-if-not
              (lambda (section)
                (eq (agent-switch-section-type section) 'client))
              (agent-switch--visible-sections))))
        (should clients)
        (dolist (section clients)
          (let ((end (agent-switch-section-end section)))
            (should-not (equal (buffer-substring-no-properties
                                (max (point-min) (- end 2)) end)
                               "\n\n"))))
        (should (= (agent-switch-section-end (car (last clients)))
                   (point-max)))))))

(ert-deftest agent-switch-client-headings-use-names-only ()
  (agent-switch-test--with-root root
    (with-temp-buffer
      (agent-switch-mode)
      (agent-switch-refresh)
      (dolist (section
               (cl-remove-if-not
                (lambda (candidate)
                  (eq (agent-switch-section-type candidate) 'client))
                (agent-switch--visible-sections)))
        (let* ((client (agent-switch-section-value section))
               (label (agent-switch-client-name client))
               (start (agent-switch-section-start section)))
          (goto-char start)
          (should (equal label
                         (buffer-substring-no-properties
                          start (line-end-position))))
          (let ((face (get-text-property start 'face)))
            (should (if (listp face)
                        (memq 'agent-switch-key face)
                      (eq face 'agent-switch-key)))))))))

(ert-deftest agent-switch-profile-rows-show-bounded-core-columns ()
  (agent-switch-test--with-root root
    (let* ((client (agent-switch-get-client "claude"))
           (profile (agent-switch-test--profile
                     "claude" "row-id"
                     (agent-switch-test--hash
                      "env" (agent-switch-test--hash
                             "ANTHROPIC_MODEL" "claude-sonnet"
                             "ANTHROPIC_BASE_URL" "https://relay.test"))
                     "Row Name"))
           (view (agent-switch--make-client-view
                  :client client :profiles (list profile)
                  :current-profile profile)))
      (setf (agent-switch-profile-setup-required-p profile) t)
      (with-temp-buffer
        (setq-local agent-switch--sections (make-hash-table :test #'equal))
        (setq-local agent-switch--visibility (make-hash-table :test #'equal))
        (agent-switch--insert-profile-section view profile)
        (let ((row (string-trim-right (buffer-string))))
          (should (string-prefix-p "  *" row))
          (should (equal (substring row 4 30)
                         "Row Name (action required)"))
          (should (equal (substring row 33 39) "row-id"))
          (should (equal (substring row 54 67) "claude-sonnet"))
          (should (equal (substring row 87) "https://relay.test"))
          (should (<= (string-width row) 129))
          (goto-char (point-min))
          (search-forward "(action required)")
          (let ((face (get-text-property (match-beginning 0) 'face)))
            (should (if (listp face)
                        (memq 'agent-switch-action-required face)
                      (eq face 'agent-switch-action-required))))
          (should (eq (face-attribute
                       'agent-switch-action-required :weight nil t)
                      'bold))
          (should (eq (face-attribute
                       'agent-switch-action-required :inherit nil t)
                      'error)))))))

(ert-deftest agent-switch-profile-row-tracks-missing-auth-source-entry ()
  (agent-switch-test--with-root root
    (let* ((reference
            (agent-switch-test--secret-reference
             "relay.example.com" "provider.options.apiKey"))
           (profile
            (agent-switch-test--profile
             "opencode" "auth-profile"
             (agent-switch-test--hash
              "provider-id" "relay"
              "model" "relay/model"
              "provider" (agent-switch-test--hash
                          "options" (agent-switch-test--hash
                                     "apiKey" reference)))
             "default")))
      (should-not (agent-switch-profile-setup-required-p profile))
      (cl-letf (((symbol-function 'auth-source-search)
                 (lambda (&rest _arguments)
                   (should-not auth-source-do-cache)
                   nil)))
        (should (string-match-p
                 "(action required)"
                 (substring-no-properties
                  (agent-switch--profile-name-cell profile)))))
      (cl-letf (((symbol-function 'auth-source-search)
                 (lambda (&rest _arguments)
                   (should-not auth-source-do-cache)
                   (list (list :host "relay.example.com"
                               :user "provider.options.apiKey"
                               :secret "available")))))
        (should-not (string-match-p
                     "(action required)"
                     (substring-no-properties
                      (agent-switch--profile-name-cell profile))))))))

(ert-deftest agent-switch-profile-row-marks-codex-without-credential ()
  (agent-switch-test--with-root root
    (let ((profile
           (agent-switch-test--profile
            "codex" "legacy"
            (agent-switch-test--hash
             "provider-id" "openai" "model" "gpt-test"
             "provider" (agent-switch-test--hash))
            "legacy")))
      (should (string-match-p
               "(action required)"
               (substring-no-properties
                (agent-switch--profile-name-cell profile)))))))

(ert-deftest agent-switch-builtins-extract-profile-columns ()
  (agent-switch-test--with-root root
    (dolist (case
             (list
              (list "claude"
                    (agent-switch-test--hash
                     "env" (agent-switch-test--hash
                            "ANTHROPIC_BASE_URL" "https://claude.test"))
                    "-" "https://claude.test")
              (list "codex"
                    (agent-switch-test--hash
                     "provider-id" "openai" "model" "gpt-5.6-sol"
                     "provider" (agent-switch-test--hash))
                    "gpt-5.6-sol" "-")
              (list "opencode"
                    (agent-switch-test--hash
                     "provider-id" "openai" "model" "openai/gpt-5.6-sol"
                     "provider" (agent-switch-test--hash
                                 "options" (agent-switch-test--hash
                                            "baseURL" "https://open.test")))
                    "openai/gpt-5.6-sol" "https://open.test")))
      (pcase-let ((`(,client-id ,payload ,model ,base-url) case))
        (let* ((client (agent-switch-get-client client-id))
               (profile (agent-switch-test--profile
                         client-id "profile-id" payload))
               (columns (agent-switch--profile-columns client profile)))
          (should (equal (plist-get columns :model) model))
          (should (equal (plist-get columns :base-url) base-url)))))
    (agent-switch-define-adapter no-columns-adapter
      :current (lambda (_client _context) nil)
      :activate (lambda (_client _profile _context) t))
    (let* ((client (agent-switch-register-client
                    'no-columns-client :adapter 'no-columns-adapter))
           (profile (agent-switch-test--profile
                     "no-columns-client" "empty"
                     (agent-switch-test--hash)))
           (columns (agent-switch--profile-columns client profile)))
      (should (equal (plist-get columns :model) "-"))
      (should (equal (plist-get columns :base-url) "-")))))

(ert-deftest agent-switch-profile-columns-enforce-maximum-widths ()
  (agent-switch-test--with-root root
    (let* ((client (agent-switch-get-client "codex"))
           (long (make-string 100 ?x))
           (profile (agent-switch-test--profile
                     "codex" long
                     (agent-switch-test--hash
                      "provider-id" "openai" "model" long
                      "provider" (agent-switch-test--hash "base_url" long))
                     long))
           (view (agent-switch--make-client-view
                  :client client :profiles (list profile))))
      (setf (agent-switch-profile-setup-required-p profile) t)
      (with-temp-buffer
        (setq-local agent-switch--sections (make-hash-table :test #'equal))
        (setq-local agent-switch--visibility (make-hash-table :test #'equal))
        (agent-switch--insert-profile-section view profile)
        (let ((row (string-trim-right (buffer-string))))
          (should (string-match-p "(action required)" row))
          (should (<= (string-width row) 129)))))))

(ert-deftest agent-switch-status-preamble-omits-last-operation-errors ()
  (agent-switch-test--with-root root
    (with-temp-buffer
      (agent-switch-mode)
      (let ((agent-switch--last-error "Verification failed")
            (inhibit-read-only t))
        (agent-switch--insert-status))
      (should-not (string-match-p "Last operation" (buffer-string)))
      (should-not (string-match-p "Verification failed" (buffer-string)))
      (goto-char (point-min))
      (search-forward "Data: ")
      (should (eq (get-text-property (point) 'face) 'default)))))

(ert-deftest agent-switch-activation-failure-is-message-only ()
  (agent-switch-test--with-root root
    (agent-switch-define-adapter message-only-adapter
      :current (lambda (_client _context) nil)
      :activate (lambda (_client _profile _context) t))
    (let* ((client (agent-switch-register-client
                    'message-only-client :adapter 'message-only-adapter))
           (profile (agent-switch-test--profile
                     "message-only-client" "failure"
                     (agent-switch-test--hash "model" "unused")))
           logged)
      (with-temp-buffer
        (agent-switch-mode)
        (agent-switch-refresh)
        (cl-letf (((symbol-function 'agent-switch-state-unprotected-confirmed-p)
                   (lambda (_adapter-id) t))
                  ((symbol-function 'agent-switch-activation-job)
                   (lambda (_client _profile &optional _interactivep)
                     (agent-switch-job-create
                      :starter
                      (lambda (_resolve reject)
                        (funcall reject
                                 '(agent-switch-error
                                   "Verification failed"))))))
                  ((symbol-function 'message)
                   (lambda (format-string &rest arguments)
                     (setq logged (apply #'format format-string arguments)))))
          (agent-switch--activate-profile client profile))
        (should (string-match-p "Verification failed" logged))
        (should-not (string-match-p "Last operation" (buffer-string)))
        (should-not (string-match-p "Verification failed"
                                    (buffer-string)))))))

(ert-deftest agent-switch-dashboard-uses-standard-line-highlighting ()
  (with-temp-buffer
    (agent-switch-mode)
    (should hl-line-mode)))

(ert-deftest agent-switch-profile-rows-are-leaves ()
  (agent-switch-test--with-root root
    (agent-switch-save-profile
     (agent-switch-test--profile
      "claude" "leaf"
      (agent-switch-test--hash
       "env" (agent-switch-test--hash
              "ANTHROPIC_BASE_URL" "https://leaf.test"))))
    (with-temp-buffer
      (agent-switch-mode)
      (agent-switch-refresh)
      (let ((section (gethash "client/claude/profile/leaf"
                              agent-switch--sections)))
        (should section)
        (should-not (agent-switch-section-expanded-p section))
        (goto-char (agent-switch-section-start section))
        (should-error (agent-switch-toggle-section) :type 'user-error)))))

(ert-deftest agent-switch-dashboard-keymap-separates-shared-and-non-evil-keys ()
  (should (eq (lookup-key agent-switch-mode-map (kbd "TAB"))
              #'agent-switch-toggle-section))
  (should (eq (lookup-key agent-switch-mode-map (kbd "RET"))
              #'agent-switch-return))
  (should (eq (lookup-key agent-switch-mode-map (kbd "g"))
              #'agent-switch-refresh))
  (should (eq (lookup-key agent-switch-mode-map (kbd "n"))
              #'agent-switch-next-section))
  (should-not (lookup-key agent-switch-mode-map (kbd "M-n")))
  (should-not (lookup-key agent-switch-mode-map (kbd "M-p")))
  (should-not (lookup-key agent-switch-mode-map (kbd "<backtab>")))
  (should-not (lookup-key agent-switch-mode-map (kbd "s"))))

(ert-deftest agent-switch-menu-exposes-adopt-and-omits-edit-suffix ()
  (should (commandp #'agent-switch-adopt-current-at-point))
  (should-not (fboundp 'agent-switch-import-current))
  (should (commandp #'agent-switch-profile-edit))
  (should (transient-get-suffix 'agent-switch-menu "A"))
  (should-error (transient-get-suffix 'agent-switch-menu "e")))

(ert-deftest agent-switch-adopt-current-at-point-adopts-live-config ()
  (agent-switch-test--with-root root
    (let ((current (agent-switch-test--hash "model" "live"))
          opened)
      (agent-switch-define-adapter adopt-adapter
        :current (lambda (_client _context) current)
        :activate (lambda (_client _profile _context) t)
        :capture-current (lambda (_client value _context)
                           (agent-switch-json-copy value)))
      (let ((client (agent-switch-register-client
                     'adopt-client :adapter 'adopt-adapter :name "Adopt")))
        (with-temp-buffer
          (agent-switch-mode)
          (cl-letf (((symbol-function 'agent-switch--client-at-point)
                     (lambda (&optional _noerror) client))
                    ((symbol-function 'agent-switch--random-profile-id)
                     (lambda (_client-id) "p-adopted"))
                    ((symbol-function 'agent-switch--read-profile-name)
                     (lambda (&optional _default) "Adopted Live"))
                    ((symbol-function 'find-file)
                     (lambda (path) (setq opened path))))
            (agent-switch-adopt-current-at-point)))
        (let ((profile (agent-switch-find-profile
                        "adopt-client" "p-adopted")))
          (should (equal opened (agent-switch-profile-source profile)))
          (should (equal (gethash "model" (agent-switch-profile-payload profile))
                         "live"))
          (should (equal (agent-switch-state-last-selected "adopt-client")
                         "p-adopted"))
          (should (equal
                   (gethash "source"
                            (agent-switch-state-selection "adopt-client"))
                   "adopted")))))))

(ert-deftest agent-switch-adopt-ui-delegates-to-operation ()
  (agent-switch-test--with-root root
    (let* ((current (agent-switch-test--hash "model" "live"))
           (client (agent-switch-get-client "codex"))
           (profile (agent-switch-test--profile
                     "codex" "p-adopted" current "Adopted Codex"))
           called opened)
      (setf (agent-switch-profile-source profile)
            (expand-file-name "p-adopted.json" root))
      (with-temp-buffer
        (agent-switch-mode)
        (cl-letf (((symbol-function 'agent-switch--client-at-point)
                   (lambda (&optional _noerror) client))
                  ((symbol-function 'agent-switch--client-current)
                   (lambda (_client) (list current nil nil)))
                  ((symbol-function 'agent-switch--read-profile-name)
                   (lambda (&optional _default) "Adopted Codex"))
                  ((symbol-function 'agent-switch-adopt-current)
                   (lambda (actual-client name actual-current)
                     (setq called (list actual-client name actual-current))
                     profile))
                  ((symbol-function 'find-file)
                   (lambda (path) (setq opened path))))
          (agent-switch-adopt-current-at-point)))
      (should (equal called (list client "Adopted Codex" current)))
      (should (equal opened (agent-switch-profile-source profile))))))

(ert-deftest agent-switch-capture-generates-auth-source-placeholders ()
  (agent-switch-test--with-root root
    (let* ((client (agent-switch-get-client "claude"))
           (current
            (agent-switch-test--hash
             "env" (agent-switch-test--hash
                    "ANTHROPIC_BASE_URL" "https://relay.example.com/api"
                    "ANTHROPIC_AUTH_TOKEN"
                    (agent-switch--secret-marker "do-not-store"))))
           (capture
            (funcall
             (agent-switch-adapter-callback
              (agent-switch-get-adapter 'claude) :capture-current)
             client current nil))
           (payload (agent-switch-capture-result-payload capture))
           (reference (gethash "ANTHROPIC_AUTH_TOKEN"
                               (gethash "env" payload)))
           (authinfo (gethash "authinfo" reference)))
      (dolist (entry '((claude . agent-switch--claude-capture-current)
                       (codex . agent-switch--codex-capture-current)
                       (opencode . agent-switch--opencode-capture-current)))
        (should (eq (agent-switch-adapter-callback
                     (agent-switch-get-adapter (car entry)) :capture-current)
                    (cdr entry))))
      (should (agent-switch-capture-result-complete-p capture))
      (should-not (agent-switch-capture-result-warnings capture))
      (should (equal (gethash "source" reference) "auth-source"))
      (should (equal (gethash "machine" authinfo) "relay.example.com"))
      (should (equal (gethash "login" authinfo)
                     "env.ANTHROPIC_AUTH_TOKEN"))
      (let ((comments (append (gethash "comments" authinfo) nil)))
        (should (= (length comments) 2))
        (should (string-match-p "settings\\.json" (car comments)))
        (should (string-match-p "apiKeyHelper" (cadr comments))))
      (should-not (string-match-p "do-not-store" (format "%S" payload))))))

(ert-deftest agent-switch-capture-generates-nested-auth-source-placeholders ()
  (agent-switch-test--with-root root
    (let* ((client (agent-switch-get-client "opencode"))
           (current
            (agent-switch-test--hash
             "provider" (agent-switch-test--hash
                         "options" (agent-switch-test--hash
                                    "baseURL" "https://relay.example.com/v1"
                                    "apiKey"
                                    (agent-switch--secret-marker
                                     "do-not-store")))))
           (capture
            (funcall
             (agent-switch-adapter-callback
              (agent-switch-get-adapter 'opencode) :capture-current)
             client current nil))
           (reference (gethash
                       "apiKey"
                       (gethash "options"
                                (gethash "provider"
                                         (agent-switch-capture-result-payload
                                          capture)))))
           (authinfo (gethash "authinfo" reference)))
      (should (agent-switch-capture-result-complete-p capture))
      (should (equal (gethash "source" reference) "auth-source"))
      (should (equal (gethash "machine" authinfo) "relay.example.com"))
      (should (equal (gethash "login" authinfo)
                     "provider.options.apiKey"))
      (let ((comments (append (gethash "comments" authinfo) nil)))
        (should (= (length comments) 2))
        (should (string-match-p "opencode/auth\\.json" (car comments)))
        (should (string-match-p "options\\.apiKey" (cadr comments)))))))

(ert-deftest agent-switch-capture-falls-back-to-client-id-for-authinfo-machine ()
  (agent-switch-test--with-root root
    (let* ((client (agent-switch-get-client "codex"))
           (current
            (agent-switch-test--hash
             "provider-id" "custom"
             "provider" (agent-switch-test--hash
                         "api_key"
                         (agent-switch--secret-marker "do-not-store"))))
           (capture
            (funcall
             (agent-switch-adapter-callback
              (agent-switch-get-adapter 'codex) :capture-current)
             client current nil))
           (reference
            (gethash "api_key"
                     (gethash "provider"
                              (agent-switch-capture-result-payload capture))))
           (authinfo (gethash "authinfo" reference)))
      (should (agent-switch-capture-result-complete-p capture))
      (should (equal (gethash "source" reference) "auth-source"))
      (should (equal (gethash "machine" authinfo) "codex"))
      (should (equal (gethash "login" authinfo) "provider.api_key")))))

(ert-deftest agent-switch-adopt-with-secret-placeholder-is-selected ()
  (agent-switch-test--with-root root
    (let ((current
           (agent-switch-test--hash
            "env" (agent-switch-test--hash
                   "ANTHROPIC_BASE_URL" "https://capture.test"
                   "ANTHROPIC_AUTH_TOKEN"
                   (agent-switch--secret-marker "live-secret"))))
          opened)
      (agent-switch-define-adapter incomplete-adapter
        :current (lambda (_client _context) current)
        :activate (lambda (_client _profile _context) t)
        :capture-current #'agent-switch--capture-current)
      (let ((client (agent-switch-register-client
                     'incomplete-client :adapter 'incomplete-adapter)))
        (with-temp-buffer
          (agent-switch-mode)
          (cl-letf (((symbol-function 'agent-switch--client-at-point)
                     (lambda (&optional _noerror) client))
                    ((symbol-function 'agent-switch--random-profile-id)
                     (lambda (_client-id) "p-incomplete"))
                    ((symbol-function 'agent-switch--read-profile-name)
                     (lambda (&optional _default) "Incomplete"))
                    ((symbol-function 'find-file)
                     (lambda (path) (setq opened path))))
            (agent-switch-adopt-current-at-point)))
        (let ((profile (agent-switch-find-profile
                        "incomplete-client" "p-incomplete")))
          (should opened)
          (should-not (agent-switch-profile-setup-required-p profile))
          (should (equal (agent-switch-state-last-selected "incomplete-client")
                         "p-incomplete"))
          (let* ((reference
                  (gethash "ANTHROPIC_AUTH_TOKEN"
                           (gethash "env"
                                    (agent-switch-profile-payload profile))))
                 (authinfo (gethash "authinfo" reference)))
            (should (equal (gethash "source" reference) "auth-source"))
            (should (equal (gethash "machine" authinfo) "capture.test"))
            (should (equal (gethash "login" authinfo)
                           "env.ANTHROPIC_AUTH_TOKEN"))))))))

(ert-deftest agent-switch-diagnostics-data-is-structured ()
  (agent-switch-test--with-root root
    (let ((data (agent-switch-diagnostics-data)))
      (should (hash-table-p data))
      (should (equal (gethash "state_status" data) "ok"))
      (should (vectorp (gethash "registered_clients" data)))
      (should (gethash "data_directory" data)))))

(ert-deftest agent-switch-operations-expose-noninteractive-profile-workflow ()
  (agent-switch-test--with-root root
    (let* ((client (agent-switch-get-client "claude"))
           (payload (agent-switch-test--hash
                     "env" (agent-switch-test--hash
                            "ANTHROPIC_BASE_URL" "https://operations.test")))
           profile)
      (cl-letf (((symbol-function 'agent-switch--random-profile-id)
                 (lambda (_client-id) "p-operations")))
        (setq profile
              (agent-switch-create-managed-profile
               client "Operations" payload)))
      (should (equal (agent-switch-profile-id profile) "p-operations"))
      (should (file-exists-p (agent-switch-profile-source profile)))
      (should-not (find-buffer-visiting (agent-switch-profile-source profile)))
      (agent-switch-delete-managed-profile profile)
      (should-not (file-exists-p (agent-switch-profile-source profile))))))

(ert-deftest agent-switch-delete-allows-selected-profile ()
  (agent-switch-test--with-root root
    (let* ((profile
            (agent-switch-save-profile
             (agent-switch-test--profile
              "claude" "selected"
              (agent-switch-test--hash
               "env" (agent-switch-test--hash
                      "ANTHROPIC_BASE_URL" "https://selected.test")))))
           (path (agent-switch-profile-source profile)))
      (agent-switch-state-set-last-selected "claude" "selected" profile)
      (agent-switch-delete-managed-profile profile)
      (should-not (file-exists-p path))
      (should-not (agent-switch-state-last-selected "claude")))))

(ert-deftest agent-switch-operation-policy-guards-unprotected-activation ()
  (agent-switch-test--with-root root
    (agent-switch-define-adapter unprotected-operation-adapter
      :current (lambda (_client _context)
                 (agent-switch-test--hash "value" "old"))
      :activate (lambda (_client _profile _context) t))
    (let* ((client (agent-switch-register-client
                    'unprotected-operation-client
                    :adapter 'unprotected-operation-adapter))
           (profile (agent-switch-test--profile
                     "unprotected-operation-client" "new"
                     (agent-switch-test--hash "value" "new"))))
      (should-error (agent-switch-apply-profile client profile)
                    :type 'agent-switch-validation-error)
      (should (agent-switch-job-p
               (agent-switch-apply-profile client profile t))))))

(ert-deftest agent-switch-payload-version-mismatch-is-rejected-before-activation ()
  (agent-switch-test--with-root root
    (let* ((client (agent-switch-get-client "claude"))
           (profile (agent-switch-test--profile
                     "claude" "future"
                     (agent-switch-test--hash "env" (agent-switch-test--hash)))))
      (setf (agent-switch-profile-payload-version profile) 2)
      (should-error (agent-switch-activation-job client profile)
                    :type 'agent-switch-validation-error))))

(ert-deftest agent-switch-delete-restores-profile-when-state-commit-fails ()
  (agent-switch-test--with-root root
    (let* ((profile (agent-switch-save-profile
                     (agent-switch-test--profile
                      "claude" "recover-delete"
                      (agent-switch-test--hash
                       "env" (agent-switch-test--hash
                              "ANTHROPIC_BASE_URL" "https://recover.test")))))
           (path (agent-switch-profile-source profile)))
      (cl-letf (((symbol-function 'agent-switch-state-remove-profile)
                 (lambda (_client-id _profile-id)
                   (signal 'agent-switch-conflict '("simulated state conflict")))))
        (should-error (agent-switch-delete-managed-profile profile)
                      :type 'agent-switch-conflict))
      (should (file-exists-p path))
      (should (equal (agent-switch-profile-id
                      (car (agent-switch-load-managed-profiles "claude")))
                     "recover-delete")))))

(ert-deftest agent-switch-adopt-removes-created-profile-when-state-commit-fails ()
  (agent-switch-test--with-root root
    (let ((client (agent-switch-get-client "claude")))
      (cl-letf (((symbol-function 'agent-switch--random-profile-id)
                 (lambda (_client-id) "p-recover-adopt"))
                ((symbol-function 'agent-switch-state-set-last-selected)
                 (lambda (&rest _arguments)
                   (signal 'agent-switch-conflict '("simulated state conflict")))))
        (should-error
         (agent-switch-adopt-capture
          client "Recover Adopt"
          (agent-switch-test--hash
           "env" (agent-switch-test--hash
                  "ANTHROPIC_BASE_URL" "https://recover.test")))
         :type 'agent-switch-conflict))
      (should-not (file-exists-p
                   (agent-switch-profile-path "claude" "p-recover-adopt"))))))

(ert-deftest agent-switch-return-edits-profile-at-point ()
  (let ((profile (agent-switch-test--profile
                  "claude" "editable" (agent-switch-test--hash)))
        edited
        toggled)
    (cl-letf (((symbol-function 'agent-switch--profile-at-point)
               (lambda (&optional _noerror) profile))
              ((symbol-function 'agent-switch-profile-edit)
               (lambda () (setq edited t)))
              ((symbol-function 'agent-switch-toggle-section)
               (lambda () (setq toggled t))))
      (agent-switch-return))
    (should edited)
    (should-not toggled)))

(ert-deftest agent-switch-return-toggles-non-profile-section ()
  (let (edited toggled)
    (cl-letf (((symbol-function 'agent-switch--profile-at-point)
               (lambda (&optional _noerror) nil))
              ((symbol-function 'agent-switch-profile-edit)
               (lambda () (setq edited t)))
              ((symbol-function 'agent-switch-toggle-section)
               (lambda () (setq toggled t))))
      (agent-switch-return))
    (should-not edited)
    (should toggled)))

(ert-deftest agent-switch-menu-does-not-expose-profile-detail-view ()
  (should-error (transient-get-suffix 'agent-switch-menu "RET")))

(ert-deftest agent-switch-display-width-handles-wide-profile-names ()
  (let ((cell (agent-switch--display-width "模型提供商名称" 12)))
    (should (= (string-width cell) 12))))

(ert-deftest agent-switch-profile-file-workflow-saves-and-opens-json ()
  (agent-switch-test--with-root root
    (let* ((client (agent-switch-get-client "claude"))
           (payload (agent-switch-test--hash
                     "env" (agent-switch-test--hash
                            "ANTHROPIC_BASE_URL" "https://file.test")))
           opened profile)
      (cl-letf (((symbol-function 'agent-switch--random-profile-id)
                 (lambda (_client-id) "p-1234abcd"))
                ((symbol-function 'find-file)
                 (lambda (path) (setq opened path))))
        (setq profile
              (agent-switch--save-new-profile client "File Profile" payload)))
      (should (equal (agent-switch-profile-id profile) "p-1234abcd"))
      (should (equal opened (agent-switch-profile-source profile)))
      (should (file-exists-p opened))
      (should (equal (agent-switch-profile-name
                      (agent-switch-find-profile "claude" "p-1234abcd"))
                     "File Profile")))))

(ert-deftest agent-switch-profile-edit-uses-standard-file-visit ()
  (agent-switch-test--with-root root
    (let* ((profile (agent-switch-save-profile
                     (agent-switch-test--profile
                      "claude" "editable"
                      (agent-switch-test--hash
                       "env" (agent-switch-test--hash
                              "ANTHROPIC_BASE_URL" "https://edit.test")))))
           opened)
      (cl-letf (((symbol-function 'agent-switch--profile-at-point)
                 (lambda (&optional _noerror) profile))
                ((symbol-function 'find-file)
                 (lambda (path) (setq opened path))))
        (agent-switch-profile-edit))
      (should (equal opened (agent-switch-profile-source profile))))))

(ert-deftest agent-switch-provider-modules-load-and-reregister ()
  (agent-switch-test--with-root root
    (dolist (feature '(agent-switch-adapter-utils
                       agent-switch-claude
                       agent-switch-codex
                       agent-switch-opencode))
      (should (featurep feature)))
    (agent-switch-register-builtins)
    (should (equal (mapcar #'agent-switch-client-id (agent-switch-clients))
                   '("claude" "codex" "opencode")))))

(ert-deftest agent-switch-builtins-provide-profile-templates ()
  (agent-switch-test--with-root root
    (should (equal (mapcar #'agent-switch-client-id (agent-switch-clients))
                   '("claude" "codex" "opencode")))
    (should (equal (agent-switch-client-name
                    (agent-switch-get-client "opencode"))
                   "OpenCode"))
    (should (equal (agent-switch-adapter-name
                    (agent-switch-get-adapter 'opencode))
                   "OpenCode"))
    (should (= (agent-switch-adapter-payload-version
                (agent-switch-get-adapter 'codex))
               2))
    (should-error (agent-switch-get-client "opencode-global")
                  :type 'agent-switch-error)
    (should-error (agent-switch-get-adapter 'opencode-global)
                  :type 'agent-switch-error)
    (should-error (agent-switch-get-client "gptel-default")
                  :type 'agent-switch-error)
    (dolist (client-id '("claude" "codex" "opencode"))
      (let* ((client (agent-switch-get-client client-id))
             (payload (agent-switch--new-profile-payload client)))
        (should (hash-table-p payload))))
    (let* ((claude (agent-switch--new-profile-payload
                    (agent-switch-get-client "claude")))
           (env (gethash "env" claude)))
      (should-not (gethash "ANTHROPIC_AUTH_TOKEN" env)))
    (let ((codex (agent-switch--new-profile-payload
                  (agent-switch-get-client "codex"))))
      (should (equal (gethash "wire_api" (gethash "provider" codex))
                     "responses"))
      (should-not (gethash "env_key" (gethash "provider" codex)))
      (should (equal (gethash "delivery" (gethash "credential" codex))
                     "command")))))

(ert-deftest agent-switch-apply-records-payload-snapshot ()
  (agent-switch-test--with-root root
    (let* ((payload (agent-switch-test--hash
                     "env" (agent-switch-test--hash
                            "ANTHROPIC_BASE_URL" "https://snapshot.test")))
           (profile (agent-switch-test--profile
                     "claude" "snapshot" payload)))
      (agent-switch-state-set-last-selected "claude" "snapshot" profile)
      (let ((applied (agent-switch-state-applied-profile "claude")))
        (should (hash-table-p applied))
        (should-not (gethash "fingerprint" applied))
        (should (equal
                 (gethash "ANTHROPIC_BASE_URL"
                          (gethash "env" (gethash "payload" applied)))
                 "https://snapshot.test"))))))

(provide 'agent-switch-test)

;;; agent-switch-test.el ends here
