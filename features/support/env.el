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
   (loop for name in '(".zeus.sock" "tmp/rake-output") do
	(when (file-exists-p (concat projectile-rails-app-path name))
	  (f-delete (concat projectile-rails-app-path name))))
   (when (-contains? (-map 'buffer-name (buffer-list)) "*projectile-rails-compilation*")
     (kill-buffer "*projectile-rails-compilation*"))
)

(After
 )

(Teardown
 )
