(require 'f)

(defvar projectile-rails-support-path
  (f-dirname load-file-name))

(defvar projectile-rails-features-path
  (f-parent projectile-rails-support-path))

(defvar projectile-rails-root-path
  (f-parent projectile-rails-features-path))

(add-to-list 'load-path projectile-rails-root-path)

(defvar projectile-rails-app-path
  (concat projectile-rails-features-path "/app/"))

(defvar projectile-rails-test-completion-buffer "*projectile-rails-test-completion*")

(defun projectile-rails-buffer-exists-p (name)
 (-contains? (-map 'buffer-name (buffer-list)) name))

(require 'projectile-rails)
(require 'espuds)
(require 'ert)

(Setup
 (setq kill-buffer-query-functions
       (remq 'process-kill-buffer-query-function
	     kill-buffer-query-functions))
 )

(Before
 (require 'yasnippet)
 (require 'projectile-rails)
 (add-hook 'projectile-mode-hook 'projectile-rails-on)
 (loop for name in '(".zeus.sock" "tmp/rake-output") do
       (when (file-exists-p (concat projectile-rails-app-path name))
	 (f-delete (concat projectile-rails-app-path name))))
 (when (projectile-rails-buffer-exists-p "*projectile-rails-compilation*")
   (kill-buffer "*projectile-rails-compilation*"))
 (setq projectile-completion-system 'ido)
 (when (projectile-rails-buffer-exists-p projectile-rails-test-completion-buffer)
   (with-current-buffer projectile-rails-test-completion-buffer
     (Given "the buffer is empty")))
 )

(After
 (kill-buffer)
 )

(Teardown
 )
