(require 'f)

(defvar projectile-rails-support-path
  (f-dirname load-file-name))

(defvar projectile-rails-features-path
  (f-parent projectile-rails-support-path))

(defvar projectile-rails-root-path
  (f-parent projectile-rails-features-path))

(add-to-list 'load-path projectile-rails-root-path)

(defvar projectile-rails-test-app-path
  (concat (make-temp-file "projectile-rails-test" t) "/"))

(defvar projectile-rails-test-completion-buffer "*projectile-rails-test-completion*")

(defun projectile-rails-buffer-exists-p (name)
 (-contains? (-map 'buffer-name (buffer-list)) name))

(defun projectile-rails-test-touch-file (filepath)
  (let ((fullpath (expand-file-name filepath projectile-rails-test-app-path)))
    (unless (file-exists-p fullpath)
      (if (s-ends-with? "/" fullpath)
	  (make-directory fullpath)
	(f-touch fullpath)))))

(require 'projectile-rails)
(require 'espuds)
(require 'ert)

(Setup
 (setq kill-buffer-query-functions
       (remq 'process-kill-buffer-query-function
	     kill-buffer-query-functions))

 (make-temp-file projectile-rails-test-app-path t)
 (cd projectile-rails-test-app-path)
 (setq projectile-indexing-method 'native)
 (loop for path in '("app/"
		     "app/assets/"
		     "app/assets/javascripts/"
		     "app/assets/stylesheets/"
		     "app/models/"
		     "app/models/admin/"
		     "app/controllers/"
		     "app/controllers/admin/"
		     "app/helpers/"
		     "app/views/"
		     "app/views/users/"
		     "app/views/admin/"
		     "app/views/admin/users/"
		     "app/jobs/"
		     "app/jobs/admin/"
		     "config/"
		     "db/"
		     "db/migrate/"
		     "lib/"
		     "lib/admin/"
		     "log/"
		     "spec/"
		     "spec/lib/"
		     "spec/models/"
		     "tmp/"
		     "Gemfile"
		     "config/environment.rb")
       do (projectile-rails-test-touch-file path))
 )

(Before
 (require 'yasnippet)
 (require 'projectile-rails)

 (add-hook 'projectile-mode-hook 'projectile-rails-on)

 (loop for name in '(".zeus.sock" "tmp/rake-output") do
       (when (file-exists-p (concat projectile-rails-test-app-path name))
	 (f-delete (concat projectile-rails-test-app-path name))))

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
 (--map (kill-buffer it) (buffer-list))
 )

(Teardown
 (delete-directory projectile-rails-test-app-path t)
 )
