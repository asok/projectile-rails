(let ((current-directory (file-name-directory load-file-name)))
  (setq projectile-rails-test-path (expand-file-name "." current-directory))
  (setq projectile-rails-root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path projectile-rails-root-path)
(add-to-list 'load-path projectile-rails-test-path)

(require 'projectile-rails)
(require 'ert-expectations)

(dolist (test-file (or argv (directory-files projectile-rails-test-path t "-test.el$")))
  (load test-file nil t))

(ert-run-tests-batch-and-exit t)
