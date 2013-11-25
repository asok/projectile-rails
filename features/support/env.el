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

 (loop for name in '("*projectile-rails-compilation*" "*projectile-rails-generate*") do
       (when (projectile-rails-buffer-exists-p name)
	 (kill-buffer name)))

 (setq projectile-completion-system 'ido
       projectile-rails-expand-snippet nil)

 (when (projectile-rails-buffer-exists-p projectile-rails-test-completion-buffer)
   (with-current-buffer projectile-rails-test-completion-buffer
     (Given "the buffer is empty")))
 )

(After
 (yas-exit-all-snippets)
 (set-buffer-modified-p nil)
 (kill-buffer)
 )

(Teardown
 )
