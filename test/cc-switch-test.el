;;; cc-switch-test.el --- Tests for cc-switch.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'sqlite)
(require 'cc-switch)

(defmacro cc-switch-test--with-temp-root (root &rest body)
  "Run BODY with ROOT bound to a temporary directory."
  (declare (indent 1))
  `(let ((,root (make-temp-file "cc-switch-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,root t))))

(defun cc-switch-test--json (value)
  "Serialize VALUE as JSON."
  (json-serialize value :null-object :null :false-object :false))

(defun cc-switch-test--hash (&rest pairs)
  "Build a JSON object hash from PAIRS."
  (let ((table (make-hash-table :test 'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(ert-deftest cc-switch-sqlite-open-file-falls-back-to-one-arg ()
  "Open SQLite files on Emacs builds without a READONLY argument."
  (let (calls)
    (cl-letf (((symbol-function 'sqlite-open)
               (lambda (&rest args)
                 (push args calls)
                 (unless (= (length args) 1)
                   (signal 'wrong-number-of-arguments
                           (list 'sqlite-open (length args))))
                 :db)))
      (should (eq (cc-switch--sqlite-open-file "/tmp/cc-switch.db" t) :db))
      (should (equal (nreverse calls)
                     '(("/tmp/cc-switch.db" t)
                       ("/tmp/cc-switch.db")))))))

(defun cc-switch-test--write-db (path rows &optional settings)
  "Create cc-switch test database PATH with provider ROWS and SETTINGS."
  (make-directory (file-name-directory path) t)
  (let ((db (cc-switch--sqlite-open-file path)))
    (unwind-protect
        (progn
          (sqlite-execute
           db
           "CREATE TABLE providers (
              id TEXT NOT NULL,
              app_type TEXT NOT NULL,
              name TEXT NOT NULL,
              settings_config TEXT NOT NULL,
              website_url TEXT,
              category TEXT,
              created_at INTEGER,
              sort_index INTEGER,
              notes TEXT,
              icon TEXT,
              icon_color TEXT,
              meta TEXT NOT NULL DEFAULT '{}',
              is_current BOOLEAN NOT NULL DEFAULT 0,
              in_failover_queue BOOLEAN NOT NULL DEFAULT 0,
              PRIMARY KEY (id, app_type))")
          (sqlite-execute db "CREATE TABLE settings (key TEXT PRIMARY KEY, value TEXT)")
          (sqlite-execute db "CREATE TABLE proxy_live_backup (app_type TEXT PRIMARY KEY, original_config TEXT NOT NULL, backed_up_at TEXT NOT NULL)")
          (sqlite-execute db "CREATE TABLE proxy_config (app_type TEXT PRIMARY KEY, enabled INTEGER NOT NULL DEFAULT 0)")
          (dolist (row rows)
            (sqlite-execute
             db
             "INSERT INTO providers
              (id, app_type, name, settings_config, website_url, category,
               created_at, sort_index, notes, icon, icon_color, meta,
               is_current, in_failover_queue)
              VALUES (?, ?, ?, ?, NULL, ?, ?, ?, NULL, NULL, NULL, ?, ?, 0)"
             (vector (plist-get row :id)
                     (plist-get row :app)
                     (plist-get row :name)
                     (plist-get row :settings)
                     (plist-get row :category)
                     (or (plist-get row :created-at) 0)
                     (or (plist-get row :sort-index) 0)
                     (or (plist-get row :meta) "{}")
                     (if (plist-get row :current) 1 0))))
          (dolist (setting settings)
            (sqlite-execute db
                            "INSERT INTO settings (key, value) VALUES (?, ?)"
                            (vector (car setting) (cdr setting)))))
      (sqlite-close db))))

(defmacro cc-switch-test--with-env (root &rest body)
  "Bind cc-switch paths under ROOT for BODY."
  (declare (indent 1))
  `(let ((cc-switch-config-dir (expand-file-name "cc-switch" ,root))
         (cc-switch-claude-config-dir (expand-file-name ".claude" ,root))
         (cc-switch-codex-home (expand-file-name ".codex" ,root)))
     ,@body))

(ert-deftest cc-switch-app-is-explicit ()
  (should-error (cc-switch-provider-current nil) :type 'cc-switch-error)
  (should-error (cc-switch-provider-switch nil "provider") :type 'cc-switch-error))

(ert-deftest cc-switch-providers-are-sorted-and-current-is-read ()
  (cc-switch-test--with-temp-root root
    (cc-switch-test--with-env root
      (cc-switch-test--write-db
       (cc-switch--db-path)
       (list
        (list :id "b" :app "claude" :name "B"
              :settings "{}" :category "relay" :created-at 2 :sort-index 2)
        (list :id "a" :app "claude" :name "A"
              :settings "{}" :category "official" :created-at 1 :sort-index 1
              :current t)))
      (should (equal (mapcar #'cc-switch--provider-id
                             (cc-switch--providers "claude"))
                     '("a" "b")))
      (should (equal (cc-switch-provider-current "claude") "a")))))

(ert-deftest cc-switch-dashboard-lists-supported-apps-with-row-context ()
  (cc-switch-test--with-temp-root root
    (cc-switch-test--with-env root
      (let ((codex-settings
             (cc-switch-test--hash "auth" (cc-switch-test--hash)
                                   "config" "model = \"gpt\"\n")))
        (cc-switch-test--write-db
         (cc-switch--db-path)
         (list
          (list :id "claude-a" :app "claude" :name "Claude A"
                :settings "{}" :category "official" :current t)
          (list :id "codex-a" :app "codex" :name "Codex A"
                :settings (cc-switch-test--json codex-settings)
                :category "official" :current t))))
      (with-temp-buffer
        (cc-switch-mode)
        (cc-switch-refresh)
        (should (equal (mapcar #'car tabulated-list-entries)
                       '(("claude" . "claude-a")
                         ("codex" . "codex-a"))))
        (should (equal (cc-switch--provider-reference-at-point)
                       '("claude" . "claude-a")))
        (should-not
         (cl-some (lambda (overlay)
                    (eq (overlay-get overlay 'face)
                        'tabulated-list-fake-header))
                  (overlays-in (point-min) (point-max))))))))

(ert-deftest cc-switch-provider-details-hide-settings-config ()
  (cc-switch-test--with-temp-root root
    (cc-switch-test--with-env root
      (let ((settings (cc-switch-test--hash
                       "env" (cc-switch-test--hash
                              "ANTHROPIC_AUTH_TOKEN" "secret-token"))))
        (cc-switch-test--write-db
         (cc-switch--db-path)
         (list (list :id "p" :app "claude" :name "Provider"
                     :settings (cc-switch-test--json settings)
                     :current t))))
      (let* ((provider (cc-switch--provider-by-id "claude" "p"))
             (details (string-join
                       (cc-switch--provider-details-lines "claude" provider)
                       "\n")))
        (should (string-match-p "Hidden: settings_config" details))
        (should-not (string-match-p "secret-token" details))))))

(ert-deftest cc-switch-switch-claude-writes-live-and-updates-db ()
  (cc-switch-test--with-temp-root root
    (cc-switch-test--with-env root
      (make-directory (cc-switch--claude-config-dir) t)
      (write-region "{\"old\":true}\n" nil (cc-switch--claude-settings-path))
      (let* ((env (cc-switch-test--hash
                   "ANTHROPIC_MODEL" "sonnet"
                   "ANTHROPIC_SMALL_FAST_MODEL" "haiku"))
             (settings (cc-switch-test--hash
                        "env" env
                        "api_format" "internal"))
             (common "{\"env\":{\"SHARED\":\"1\"},\"includeCoAuthoredBy\":false}"))
        (cc-switch-test--write-db
         (cc-switch--db-path)
         (list
          (list :id "old" :app "claude" :name "Old" :settings "{}" :current t)
          (list :id "new" :app "claude" :name "New"
                :settings (cc-switch-test--json settings)
                :meta "{\"commonConfigEnabled\":true}"))
         (list (cons "common_config_claude" common)))
        (cc-switch-provider-switch "claude" "new")
        (let* ((written (cc-switch--parse-json
                         (with-temp-buffer
                           (insert-file-contents (cc-switch--claude-settings-path))
                           (buffer-string))))
               (written-env (gethash "env" written)))
          (should (not (gethash "api_format" written)))
          (should (equal (gethash "ANTHROPIC_DEFAULT_HAIKU_MODEL" written-env) "haiku"))
          (should (equal (gethash "ANTHROPIC_DEFAULT_SONNET_MODEL" written-env) "sonnet"))
          (should (equal (gethash "SHARED" written-env) "1"))
          (should (equal (cc-switch-provider-current "claude") "new"))
          (should (file-exists-p (cc-switch--backup-path
                                  (cc-switch--claude-settings-path)))))))))

(ert-deftest cc-switch-missing-live-dir-refuses-without-db-change ()
  (cc-switch-test--with-temp-root root
    (cc-switch-test--with-env root
      (cc-switch-test--write-db
       (cc-switch--db-path)
       (list
        (list :id "old" :app "claude" :name "Old" :settings "{}" :current t)
        (list :id "new" :app "claude" :name "New" :settings "{}")))
      (should-error (cc-switch-provider-switch "claude" "new") :type 'cc-switch-error)
      (should (equal (cc-switch-provider-current "claude") "old")))))

(ert-deftest cc-switch-codex-missing-toml-soft-fails ()
  (cc-switch-test--with-temp-root root
    (cc-switch-test--with-env root
      (make-directory (cc-switch--codex-home) t)
      (let ((settings (cc-switch-test--hash "auth" (cc-switch-test--hash)
                                            "config" "model = \"gpt\"\n")))
        (cc-switch-test--write-db
         (cc-switch--db-path)
         (list
          (list :id "old" :app "codex" :name "Old"
                :settings (cc-switch-test--json settings) :current t)
          (list :id "new" :app "codex" :name "New"
                :settings (cc-switch-test--json settings)))))
      (let ((features (remq 'toml features)))
        (should-error (cc-switch-provider-switch "codex" "new")
                      :type 'cc-switch-error))
      (should (equal (cc-switch-provider-current "codex") "old")))))

(ert-deftest cc-switch-codex-third-party-preserves-auth ()
  (cc-switch-test--with-temp-root root
    (cc-switch-test--with-env root
      (make-directory (cc-switch--codex-home) t)
      (write-region "{\"tokens\":{\"id\":\"oauth\"}}\n" nil (cc-switch--codex-auth-path))
      (write-region ";;; toml.el --- Test stub -*- lexical-binding: t; -*-\n(provide 'toml)\n"
                    nil
                    (expand-file-name "toml.el" root))
      (let ((load-path (cons root load-path))
            (settings (cc-switch-test--hash
                       "auth" (cc-switch-test--hash "OPENAI_API_KEY" "secret")
                       "config" "model = \"gpt-5\"\n")))
        (cc-switch-test--write-db
         (cc-switch--db-path)
         (list
          (list :id "old" :app "codex" :name "Old"
                :settings (cc-switch-test--json settings) :current t)
          (list :id "new" :app "codex" :name "New"
                :settings (cc-switch-test--json settings)
                :category "relay")))
        (cc-switch-provider-switch "codex" "new")
        (should (equal (with-temp-buffer
                         (insert-file-contents (cc-switch--codex-auth-path))
                         (buffer-string))
                       "{\"tokens\":{\"id\":\"oauth\"}}\n"))
        (should (equal (with-temp-buffer
                         (insert-file-contents (cc-switch--codex-config-path))
                         (buffer-string))
                       "model = \"gpt-5\"\n"))
        (should (equal (cc-switch-provider-current "codex") "new"))))))

(ert-deftest cc-switch-diagnose-is-sanitized ()
  (cc-switch-test--with-temp-root root
    (cc-switch-test--with-env root
      (let ((settings (cc-switch-test--hash
                       "env" (cc-switch-test--hash "ANTHROPIC_AUTH_TOKEN" "secret-token"))))
        (cc-switch-test--write-db
         (cc-switch--db-path)
         (list (list :id "p" :app "claude" :name "Provider"
                     :settings (cc-switch-test--json settings)
                     :current t))))
      (let ((diagnose (cc-switch-diagnose)))
        (should (string-match-p "database exists: yes" diagnose))
        (should-not (string-match-p "secret-token" diagnose))))))

(provide 'cc-switch-test)

;;; cc-switch-test.el ends here
