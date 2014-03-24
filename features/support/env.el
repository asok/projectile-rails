(require 'f)

(defvar projectile-rails-support-path
  (f-dirname load-file-name))

(defvar projectile-rails-features-path
  (f-parent projectile-rails-support-path))

(defvar projectile-rails-root-path
  (f-parent projectile-rails-features-path))

(add-to-list 'load-path projectile-rails-root-path)

(defvar projectile-rails-test-app-path
  (f-canonical (concat (make-temp-file "projectile-rails-test" t) "/")))

(defvar projectile-rails-test-spring-pid-file
  (concat
   temporary-file-directory
   "spring/"
   (md5 projectile-rails-test-app-path 0 -1)
   ".pid"))

(defvar projectile-rails-test-zeus-pid-file
  (concat projectile-rails-test-app-path ".zeus.sock"))

(defvar projectile-rails-test-rake-cache-file
  (concat projectile-rails-test-app-path "/tmp/rake-output"))

(defun projectile-rails-test-touch-file (filepath)
  (let ((fullpath (expand-file-name filepath projectile-rails-test-app-path)))
    (unless (file-exists-p fullpath)
      (if (s-ends-with? "/" fullpath)
	  (make-directory fullpath)
	(f-touch fullpath)))))

(defun projectile-rails-test-create-foo-gem (dir)
  (make-directory (concat projectile-rails-test-app-path dir))
  (find-file (concat projectile-rails-test-app-path dir "foo.gemspec"))
  (insert "Gem::Specification.new do |spec|
  spec.name          = 'foo'
  spec.version       = '0.0.0'
end")
  (save-buffer)
  )

(require 'projectile-rails)
(require 'espuds)
(require 'ert)

(Setup
 (setq kill-buffer-query-functions
       (remq 'process-kill-buffer-query-function
	     kill-buffer-query-functions))

 (make-temp-file projectile-rails-test-app-path t)
 (setq projectile-indexing-method 'native)
 (loop for path in `("app/"
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
		     "app/views/layouts/"
		     "app/jobs/"
		     "app/jobs/admin/"
		     "app/mailers/"
		     "config/"
		     "config/environments/"
		     "config/initializers/"
		     "config/locales/"
		     "db/"
		     "db/migrate/"
		     "lib/"
		     "lib/admin/"
		     "lib/assets/"
		     "lib/assets/javascripts/"
		     "lib/assets/stylesheets/"
		     "public/"
		     "public/javascripts/"
		     "log/"
		     "spec/"
		     "spec/lib/"
		     "spec/models/"
		     "spec/controllers/"
		     "spec/controllers/admin/"
		     "features/"
		     "tmp/"
		     "vendor/"
		     "Gemfile"
		     "config/environment.rb"
		     ,(concat temporary-file-directory "spring/"))
       do (projectile-rails-test-touch-file path))
 )

(Before
 (require 'yasnippet)
 (require 'bundler)
 (require 'rspec-mode)
 (require 'projectile-rails)

 (add-hook 'projectile-mode-hook 'projectile-rails-on)

 (loop for file in (list projectile-rails-test-spring-pid-file
			 projectile-rails-test-zeus-pid-file
			 projectile-rails-test-rake-cache-file)
       do (when (f-exists? file) (f-delete file)))

 (setq projectile-completion-system 'ido
       projectile-rails-expand-snippet nil)

 (cd projectile-rails-test-app-path)
 )

(After
 (yas-exit-all-snippets)
 (--map (kill-buffer it) (buffer-list))
 )

(Teardown
 (delete-directory projectile-rails-test-app-path t)
 )
