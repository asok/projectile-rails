(require 'projectile)
(require 'inflections)

(defgroup projectile-rails nil
  "Rails mode based on projectile"
  :prefix "ruby-"
  :group 'projectile)

(defcustom projectile-rails-views-re
  "\\.\\(?:html\\|erb\\|haml\\|js\\|slim\\|json\\|coffee\\|css\\)$"
  "Regexp for filtering for view files"
  :group 'projectile-rails
  :type '(string))

(defvar projectile-rails-mode-hook nil
  "Hook for `projectile-rails-mode'.")

(defvar projectile-rails-mode-map (make-sparse-keymap)
  "Keymap for `projectile-rails-mode'.")

(defun projectile-rails-dir-files (dir re)
  (let ((length (length dir)))
    (--map (substring it length)
	   (--filter (string-match-p re it)
		     (projectile-dir-files (projectile-expand-root dir))))))

(defun projectile-rails--resource (prompt dir re)
  (find-file 
   (projectile-expand-root (concat dir
				   (projectile-completing-read
				    prompt
				    (projectile-rails-dir-files dir re))))))

(defun projectile-rails-models ()
  (interactive)
  (projectile-rails--resource "models: " "app/models/" "\\.rb$"))

(defun projectile-rails-controllers ()
  (interactive)
  (projectile-rails--resource "controller: " "app/controllers/" "\\.rb$"))

(defun projectile-rails-views ()
  (interactive)
  (projectile-rails--resource "views: " "app/views/" projectile-rails-views-re))

(defun projectile-rails-helpers ()
  (interactive)
  (projectile-rails--resource "helpers: " "app/helpers/" "\\.rb$"))

(defun projectile-rails-libs ()
  (interactive)
  (projectile-rails--resource "libs: " "lib/" "\\.rb$"))

(defun projectile-rails-specs ()
  (interactive)
  (projectile-rails--resource "specs: " "spec/" "_spec\\.rb$"))

(defun projectile-rails-current-resource ()
  (interactive)
  (let* ((name (projectile-rails-current-resource-name))
	 (singular (singularize-string name))
	 (plural (pluralize-string name)))
    (find-file 
     (projectile-expand-root 
      (projectile-completing-read
       (concat singular ": ")
       (--filter (string-match-p (s-lex-format "\\(${singular}\\|${plural}\\)\\(_controller\\)?\\(_spec\\)?\\.rb$") it) 
		 (projectile-current-project-files)))))))

(defun projectile-rails-current-resource-name ()
  "Returns a resource name extracted from the name of the currently visiting file"
  (let ((file-name (buffer-file-name)))
    (if file-name
	(catch 'break (loop
		       for re in '("app/models/\\(.+\\)\\.rb$"
				   "/\\([a-z_]+\\)_controller\\.rb$"
				   "app/views/\\(.+\\)/[^/]+$"
				   "app/helpers/\\(.+\\)_helper\\.rb$"
				   "lib/.*\\([a-z_]+\\)\\.rb$"
				   "spec/.*/\\([a-z_]+?\\)\\(_controller\\)?_spec\\.rb$")
		       do (if (string-match re file-name)
			      (throw 'break (match-string 1 file-name))))))
    )
  )

(defun projectile-rails-root ()
  "Returns rails root directory if this file is a part of a Rails application else nil"
  (if (file-exists-p (projectile-expand-root "config/environment.rb"))
      (projectile-project-root)
    nil))

(define-minor-mode projectile-rails-mode
  "Rails mode based on projectile"
  :init-value nil
  :lighter " ProjectileR"
  :keymap projectile-rails-mode-map)

(defadvice projectile-on (after projectile-rails-turn-on activate)
  "Run `projectile-rails-mode' if this is a rails project."
  (when (projectile-rails-root)
    (projectile-rails-mode)))

(define-derived-mode projectile-rails-compilation-mode compilation-mode "Projectile Rails Compilation"
  "Compilation mode for projectile-rails output of rails generate."
  (set (make-local-variable 'compilation-error-regexp-alist)
       (cons 'projectile-rails-generate compilation-error-regexp-alist))
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (cons '(projectile-rails-generate arm/errors-regex 1 2)
             compilation-error-regexp-alist-alist)))

(provide 'projectile-rails)
