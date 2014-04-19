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
 (make-temp-file projectile-rails-test-app-path t)
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

 (require 'yasnippet)
 (require 'bundler)
 (require 'rspec-mode)
 (require 'projectile-rails)

 (add-hook 'projectile-mode-hook 'projectile-rails-on)

 (setq kill-buffer-query-functions nil
       projectile-completion-system 'ido
       projectile-indexing-method 'native
       projectile-rails-expand-snippet nil)

 (cd projectile-rails-test-app-path)
 )

(Before
 (loop for file in (list projectile-rails-test-spring-pid-file
			 projectile-rails-test-zeus-pid-file
			 projectile-rails-test-rake-cache-file)
       do (when (f-exists? file) (f-delete file)))
 )

(After
 (yas-exit-all-snippets)
 (--each (buffer-list)
   (with-current-buffer it
     (when projectile-rails-mode
       (kill-buffer))))
 )

(Teardown
 ;;todo: come up with something that works
 ;;some scenario is hanging when spring executable is present
 (start-process-shell-command "spring" nil "spring stop")
 (delete-directory projectile-rails-test-app-path t)
 )
