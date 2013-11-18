(When "^I open the app file \"\\(.+\\)\""
      (lambda (filename)
	(find-file (concat projectile-rails-app-path "/" filename))))

(When "^I open the file \"\\(.+\\)\""
      (lambda (filename) (find-file (concat projectile-rails-root-path "/" filename))))

(When "^I turn on projectile-mode"
      (lambda () (projectile-on)))

(When "^I run command \"\\(.+\\)\" selecting \"\\(.+\\)\"$"
      (lambda (command argument)
	(When "I start an action chain")
	(When "I press \"M-x\"")
	(And (s-lex-format "I type \"${command}\""))
	(When "I press \"RET\"")
	(And (s-lex-format "I type \"${argument}\""))
	(And "I execute the action chain")
	)
      )

(Then "^projectile-rails should be turned on"
      (lambda () (should projectile-rails-mode)))

(Then "^projectile-rails should not be turned on"
      (lambda () (should-not projectile-rails-mode)))

(Then "^I am in file \"\\(.+\\)\""
      (lambda (filename)
	(print buffer-file-name)
	(should (string-match-p (s-lex-format "${filename}$") (buffer-file-name)))))
