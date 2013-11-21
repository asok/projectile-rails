(When "^I open the app file \"\\(.+\\)\""
      (lambda (filename)
	(find-file (concat projectile-rails-app-path filename))))

(When "^I open the file \"\\(.+\\)\""
      (lambda (filename) (find-file (concat projectile-rails-root-path "/" filename))))

(When "^I turn on projectile-mode"
      (lambda ()
	(projectile-on)))

(When "^I turn off projectile-rails-mode"
      (lambda ()
	(projectile-rails-off)))

(When "^I turn on ruby-mode"
      (lambda () (ruby-mode)))

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

(When "^I run \"\\(.+\\)\""
      (lambda (command)
	(When "I start an action chain")
	(When "I press \"M-x\"")
	(And (s-lex-format "I type \"${command}\""))
	(And "I execute the action chain")))

(When "^I sleep for \\([0-9]+\\) seconds"
      (lambda(seconds) (sit-for (string-to-int seconds))))

(Given "^zeus is running"
       (lambda ()
	 (f-touch (concat projectile-rails-app-path ".zeus.sock"))))

(Given "the cache file with projectile-rails task exists"
       (lambda ()
	 (with-temp-file (concat projectile-rails-app-path "tmp/rake-output")
	   (insert "rake projectile-rails #a test task\n"))))

(Then "^projectile-rails should be turned on"
      (lambda () (should projectile-rails-mode)))

(Then "^projectile-rails should not be turned on"
      (lambda () (should-not projectile-rails-mode)))

(Then "^I am in file \"\\(.+\\)\""
      (lambda (filename)
	(should (string-match-p (s-lex-format "${filename}$") (buffer-file-name)))))

(Then "^I should see \"\\(.+\\)\" font locked"
      (lambda (keyword)
	(When (s-lex-format "I go to word \"${keyword}\""))
	(should (equal (get-text-property (+ (point) 1) 'face) 'font-lock-keyword-face))))

(Then "^the buffer is auto reverting"
      (lambda ()
	(should (and auto-revert-tail-mode (not auto-revert-verbose)))))

(Then "^the completions should be:"
      (lambda (text)
	(with-current-buffer projectile-rails-test-completion-buffer
	  (Then "I should see:" text)
	  (should (= (length text) (buffer-size))))))

(And "I exit the snippets"
     (lambda ()
       (yas-exit-all-snippets)))
