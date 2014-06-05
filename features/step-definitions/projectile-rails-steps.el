(When "^I open the app file \"\\(.+\\)\""
      (lambda (filename)
  (find-file (concat projectile-rails-test-app-path "/" filename))))

(When "^I open the file \"\\(.+\\)\""
      (lambda (filename) (find-file (concat projectile-rails-root-path "/" filename))))

(When "^I turn on projectile-mode"
      (lambda ()
  (projectile-on)))

(When "^I turn off projectile-rails-mode"
      (lambda ()
  (projectile-rails-off)))

(When "^I run command \"\\(.+\\)\" \\(?:selecting\\|inputting\\) \"\\(.+\\)\"$"
      (lambda (command argument)
  (When "I start an action chain")
  (When "I press \"M-x\"")
  (And (s-lex-format "I type \"${command}\""))
  (When "I press \"RET\"")
  (And (s-lex-format "I type \"${argument}\""))
  (And "I execute the action chain")))

(When "^I force font lock refresh"
      (lambda()
  (font-lock-fontify-buffer)))

(Given "^I am using a test completion system$"
      (lambda ()
  (setq projectile-completion-system
        (lambda (prompt choices)
    (with-current-buffer (get-buffer-create projectile-rails-test-completion-buffer)
      (insert (s-join "\n" choices)))))))

(Given "^I turn on snippet expansion"
      (lambda ()
  (setq projectile-rails-expand-snippet t)))

(Given "^I turn off snippet expansion"
      (lambda ()
  (setq projectile-rails-expand-snippet nil)))

(Given "^I turn off adding keywords"
      (lambda ()
  (setq projectile-rails-add-keywords nil)))

(When "^I run \"\\(.+\\)\""
      (lambda (command)
  (When "I start an action chain")
  (When "I press \"M-x\"")
  (And (s-lex-format "I type \"${command}\""))
  (And "I execute the action chain")))

(When "^I sleep for \\([0-9]+\\) seconds"
      (lambda(seconds) (sit-for (string-to-int seconds))))

(Given "^spring is running"
       (lambda ()
   (f-touch projectile-rails-test-spring-pid-file)))

(Given "^zeus is running"
       (lambda ()
   (f-touch projectile-rails-test-zeus-pid-file)))

(Given "the cache file with projectile-rails task exists"
       (lambda ()
   (with-temp-file projectile-rails-test-rake-cache-file
     (insert "rake projectile-rails #a test task\n"))))

(Then "^projectile-rails should be turned on"
      (lambda ()
  (should projectile-rails-mode)))

(Then "^projectile-rails should not be turned on"
      (lambda () (should-not projectile-rails-mode)))

(Then "^I am in file \"\\(.+\\)\""
      (lambda (filename)
  (should (string-match-p (s-lex-format "${filename}$") (buffer-file-name)))))

(Then "^I should see \"\\(.+\\)\" font locked"
      (lambda (keyword)
  (When (s-lex-format "I go to word \"${keyword}\""))
  (should (equal (get-text-property (+ (point) 1) 'face) 'font-lock-function-name-face))))

(Then "^I should not see \"\\(.+\\)\" font locked"
      (lambda (keyword)
  (When (s-lex-format "I go to word \"${keyword}\""))
  (should (not (equal (get-text-property (+ (point) 1) 'face) 'font-lock-function-name-face)))))

(Then "^the buffer is auto reverting"
      (lambda ()
  (should (and auto-revert-tail-mode (not auto-revert-verbose)))))

(And "I exit the snippets"
     (lambda ()
       (yas-exit-all-snippets)))

(Then "^I am in a dired buffer \"\\(.+\\)\""
      (lambda (name)
  (should (string=
     (dired-current-directory)
     (projectile-expand-root name)))))

(And "I wait for \\([0-9]+\\) seconds"
     (lambda (seconds)
       (sit-for (string-to-int seconds))))

(And "I print the buffer content"
     (lambda ()
       (print (buffer-string))))

(And "I clear the buffer and insert:"
     (lambda (content)
       (When "I clear the buffer")
       (When "I insert:" content)))

(When "I set read-only to false"
      (lambda ()
  (read-only-mode -1)))

(Then "I should be at line \\([0-9]+\\)"
      (lambda (line)
  (should (= (count-lines 1 (point)) (string-to-int line)))))

(When "I simulate running server with output:"
      (lambda (output)
  (When "I run \"projectile-rails-server\"")
  (And "I switch to buffer \"*projectile-rails-server*\"")
  (And "I set read-only to false")
  (When "I clear the buffer")
  (compilation-filter (get-buffer-process (current-buffer)) output)))

(When "I simulate generating \"\\(.*\\)\" with output:"
      (lambda (arg output)
  (When (s-lex-format "I run command \"projectile-rails-generate\" inputting \"${arg}\""))
  (And "I switch to buffer \"*projectile-rails-generate*\"")
  (And "I set read-only to false")
  (When "I clear the buffer")
  (compilation-filter (get-buffer-process (current-buffer)) output)
  (run-hook-with-args 'compilation-finish-functions (current-buffer) "")))

(Given "\\(?:directory\\|file\\) \"\\(.*\\)\" exists"
       (lambda (filepath)
   (projectile-rails-test-touch-file filepath)))

(Given "there is foo gem in directory \"\\(.+\\)\""
       (lambda (dir)
   (projectile-rails-test-create-foo-gem dir)))

(Given "I save the buffer"
       (lambda ()
   (save-buffer)))

(Then "^key \"\\(.+\\)\" should be mapped to \"\\(.+\\)\"$"
      (lambda (key command)
  (should (equal (key-binding (kbd key)) (intern command)))))

(And "I debug"
     (lambda ()
       (print (buffer-string))))
