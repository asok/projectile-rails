(require 'projectile)
(require 'inf-ruby)
(require 'inflections)

(defgroup projectile-rails nil
  "Rails mode based on projectile"
  :prefix "ruby-"
  :group 'projectile)

(defcustom projectile-rails-controller-keywords
  '("logger" "polymorphic_path" "polymorphic_url" "mail" "render" "attachments"
    "default" "helper" "helper_attr" "helper_method" "layout" "url_for"
    "serialize" "exempt_from_layout" "filter_parameter_logging" "hide_action"
    "cache_sweeper" "protect_from_forgery" "caches_page" "cache_page"
    "caches_action" "expire_page" "expire_action" "rescue_from" "params"
    "request" "response" "session" "flash" "head" "redirect_to"
    "render_to_string" "respond_with" "before_filter" "append_before_filter"
    "prepend_before_filter" "after_filter" "append_after_filter"
    "prepend_after_filter" "around_filter" "append_around_filter"
    "prepend_around_filter" "skip_before_filter" "skip_after_filter" "skip_filter")
  "List of keywords to highlight for controllers"
  :group 'projectile-rails
  :type '(repeat string))

(defcustom projectile-rails-migration-keywords
  '("create_table" "change_table" "drop_table" "rename_table" "add_column"
    "rename_column" "change_column" "change_column_default" "remove_column"
    "add_index" "remove_index" "rename_index" "execute")
  "List of keywords to highlight for migrations"
  :group 'projectile-rails
  :type '(repeat string))

(defcustom projectile-rails-model-keywords
  '("default_scope" "named_scope" "scope" "serialize" "belongs_to" "has_one"
    "has_many" "has_and_belongs_to_many" "composed_of" "accepts_nested_attributes_for"
    "before_create" "before_destroy" "before_save" "before_update" "before_validation"
    "before_validation_on_create" "before_validation_on_update" "after_create"
    "after_destroy" "after_save" "after_update" "after_validation"
    "after_validation_on_create" "after_validation_on_update" "around_create"
    "around_destroy" "around_save" "around_update" "after_commit" "after_find"
    "after_initialize" "after_rollback" "after_touch" "attr_accessible"
    "attr_protected" "attr_readonly" "validates" "validate" "validate_on_create"
    "validate_on_update" "validates_acceptance_of" "validates_associated"
    "validates_confirmation_of" "validates_each" "validates_exclusion_of"
    "validates_format_of" "validates_inclusion_of" "validates_length_of"
    "validates_numericality_of" "validates_presence_of" "validates_size_of"
    "validates_existence_of" "validates_uniqueness_of" "validates_with")
  "List of keywords to highlight for models"
  :group 'projectile-rails
  :type '(repeat string))

(defcustom projectile-rails-view-keywords
  '("action_name" "atom_feed" "audio_path" "audio_tag" "auto_discovery_link_tag"
    "button_tag" "button_to" "button_to_function" "cache" "capture" "cdata_section"
    "check_box" "check_box_tag" "collection_select" "concat" "content_for"
    "content_tag" "content_tag_for" "controller" "controller_name"
    "controller_path" "convert_to_model" "cookies" "csrf_meta_tag" "csrf_meta_tags"
    "current_cycle" "cycle" "date_select" "datetime_select" "debug"
    "distance_of_time_in_words" "distance_of_time_in_words_to_now" "div_for"
    "dom_class" "dom_id" "email_field" "email_field_tag" "escape_javascript"
    "escape_once" "excerpt" "favicon_link_tag" "field_set_tag" "fields_for"
    "file_field" "file_field_tag" "flash" "form_for" "form_tag"
    "grouped_collection_select" "grouped_options_for_select" "headers"
    "hidden_field" "hidden_field_tag" "highlight" "image_alt" "image_path"
    "image_submit_tag" "image_tag" "j" "javascript_cdata_section"
    "javascript_include_tag" "javascript_path" "javascript_tag" "l" "label"
    "label_tag" "link_to" "link_to_function" "link_to_if" "link_to_unless"
    "link_to_unless_current" "localize" "logger" "mail_to" "number_field"
    "number_field_tag" "number_to_currency" "number_to_human" "number_to_human_size"
    "number_to_percentage" "number_to_phone" "number_with_delimiter"
    "number_with_precision" "option_groups_from_collection_for_select"
    "options_for_select" "options_from_collection_for_select" "params"
    "password_field" "password_field_tag" "path_to_audio" "path_to_image"
    "path_to_javascript" "path_to_stylesheet" "path_to_video" "phone_field"
    "phone_field_tag" "pluralize" "provide" "radio_button" "radio_button_tag"
    "range_field" "range_field_tag" "raw" "render" "request"
    "request_forgery_protection_token" "reset_cycle" "response" "safe_concat"
    "safe_join" "sanitize" "sanitize_css" "search_field" "search_field_tag"
    "select" "select_date" "select_datetime" "select_day" "select_hour"
    "select_minute" "select_month" "select_second" "select_tag" "select_time"
    "select_year" "session" "simple_format" "strip_links" "strip_tags"
    "stylesheet_link_tag" "stylesheet_path" "submit_tag" "t" "tag" "telephone_field"
    "telephone_field_tag" "text_area" "text_area_tag" "text_field" "text_field_tag"
    "time_ago_in_words" "time_select" "time_tag" "time_zone_options_for_select"
    "time_zone_select" "translate" "truncate" "url_field" "url_field_tag"
    "url_for" "url_options" "video_path" "video_tag" "word_wrap")
  "List of keywords to highlight for views"
  :group 'projectile-rails
  :type '(repeat string))

(defcustom projectile-rails-active-support-keywords
  '("alias_attribute" "with_options" "delegate")
  "List of keywords to highlight for all `projectile-rails-mode' buffers"
  :group 'projectile-rails
  :type '(repeat string))

(defcustom projectile-rails-font-lock-face-name 'font-lock-keyword-face
  "Face to be used for higlighting rails the keywords")

(defcustom projectile-rails-views-re
  "\\.\\(?:html\\|erb\\|haml\\|js\\|slim\\|json\\|coffee\\|css\\)$"
  "Regexp for filtering for view files"
  :group 'projectile-rails
  :type 'string)

(defcustom projectile-rails-errors-regex
  "\\([0-9A-Za-z@_./\:-]+\\.rb\\):?\\([0-9]+\\)?"
  "The regex used to find errors with file paths."
  :group 'projectile-rails
  :type 'string)

(defvar projectile-rails-mode-hook nil
  "Hook for `projectile-rails-mode'.")

(defvar projectile-rails-mode-map (make-sparse-keymap)
  "Keymap for `projectile-rails-mode'.")

(defun projectile-rails--highlight-keywords (keywords)
  "Highlight the passed KEYWORDS in current buffer."
  (font-lock-add-keywords
   nil
   (list (list
          (concat "\\(^\\|[^_:.@$]\\|\\.\\.\\)\\b"
                  (regexp-opt keywords t)
                  ruby-keyword-end-re)
          (list 2 projectile-rails-font-lock-face-name)))))

(defun projectile-rails-apply-keywords-for-file-type ()
  "Apply extra font lock keywords specific to models, controllers etc."
  (loop for (re keywords) in `(("_controller\\.rb$"   ,projectile-rails-controller-keywords)
                               ("app/models/.+\\.rb$" ,projectile-rails-model-keywords)
                               ("db/migrate/.+\\.rb$" ,projectile-rails-migration-keywords))
        do (when (and (buffer-file-name) (string-match-p re (buffer-file-name)))
             (projectile-rails--highlight-keywords
	      (append keywords projectile-rails-active-support-keywords)))))

(defun projectile-rails-dir-files (dir re)
  (let ((length (length dir)))
    (--map (substring it length)
	   (--filter (string-match-p re it)
		     (projectile-dir-files (projectile-expand-root dir))))))

(defun projectile-rails-find-file-in-dir (prompt dir re)
  (find-file 
   (projectile-expand-root (concat dir
				   (projectile-completing-read
				    prompt
				    (projectile-rails-dir-files dir re))))))

(defun projectile-rails-models ()
  (interactive)
  (projectile-rails-find-file-in-dir "models: " "app/models/" "\\.rb$"))

(defun projectile-rails-controllers ()
  (interactive)
  (projectile-rails-find-file-in-dir "controller: " "app/controllers/" "\\.rb$"))

(defun projectile-rails-views ()
  (interactive)
  (projectile-rails-find-file-in-dir "views: " "app/views/" projectile-rails-views-re))

(defun projectile-rails-helpers ()
  (interactive)
  (projectile-rails-find-file-in-dir "helpers: " "app/helpers/" "\\.rb$"))

(defun projectile-rails-libs ()
  (interactive)
  (projectile-rails-find-file-in-dir "libs: " "lib/" "\\.rb$"))

(defun projectile-rails-specs ()
  (interactive)
  (projectile-rails-find-file-in-dir "specs: " "spec/" "_spec\\.rb$"))

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

(defun projectile-rails-find-log ()
  (interactive)
  (projectile-rails-find-file-in-dir "log: " "log/" "\\.log$")
  (auto-revert-tail-mode +1)
  (setq-local auto-revert-verbose nil)
  (buffer-disable-undo))

(defun projectile-rails-rake-tmp-file ()
  (projectile-expand-root "tmp/rake-output"))

(defun projectile-rails-rake-tasks ()
  "Returns a content of tmp file with rake tasks."
  (if (file-exists-p (projectile-rails-rake-tmp-file))
      (with-temp-buffer
        (insert-file-contents (projectile-rails-rake-tmp-file))
        (buffer-string))
    (projectile-rails-regenerate-rake)
    (projectile-rails-rake-tasks)))

;; Shamelessly stolen from ruby-starter-kit.el:
;; https://github.com/technomancy/emacs-starter-kit/blob/v2/modules/starter-kit-ruby.el
(defun projectile-rails-pcmpl-rake-tasks ()
  "Return a list of all the rake tasks defined in the current projects."
  (--keep it
	  (--map (if (string-match "rake \\([^ ]+\\)" it) (match-string 1 it))
		 (split-string (projectile-rails-rake-tasks) "[\n]"))))

(defun projectile-rails-regenerate-rake ()
  "Generates rakes tasks file in the tmp within rails root directory."
  (interactive)
  (if (file-exists-p (projectile-rails-rake-tmp-file)) (delete-file (projectile-rails-rake-tmp-file)))
  (with-temp-file (projectile-rails-rake-tmp-file)
    (insert
     (projectile-rails-in-root
      (shell-command-to-string
       (projectile-rails-if-zeus "zeus rake -T" "bundle exec rake -T"))))))

(defun projectile-rails-rake (task)
  (interactive
   (list
    (projectile-completing-read
     "Rake (default: default): "
     (projectile-rails-pcmpl-rake-tasks))))
  (let ((default-directory (projectile-rails-root)))
    (projectile-rails-in-root
     (compile
      (concat
       (projectile-rails-if-zeus "zeus rake " "bundle exec rake ") (if (= 0 (length task))
                                                          "default"
                                                        task))
      'projectile-rails-compilation-mode))))

(defun projectile-rails-root ()
  "Returns rails root directory if this file is a part of a Rails application else nil"
  (if (file-exists-p (projectile-expand-root "config/environment.rb"))
      (projectile-project-root)
    nil))

(defun projectile-rails-console ()
  (interactive)
  (projectile-rails-in-root
   (with-current-buffer (run-ruby
			 (projectile-rails-if-zeus "zeus console" "bundle exec rails console"))
     (projectile-rails-mode +1))))

(defmacro projectile-rails-if-zeus (command-for-zeus command-for-bundler)
  `(if (file-exists-p (projectile-expand-root ".zeus.sock"))
       ,command-for-zeus
     ,command-for-bundler))

(defmacro projectile-rails-in-root (body-form)
  `(let ((default-directory (projectile-rails-root)))
     ,body-form))

(add-hook 'projectile-rails-mode-hook 'projectile-rails-apply-keywords-for-file-type)

(define-minor-mode projectile-rails-mode
  "Rails mode based on projectile"
  :init-value nil
  :lighter " ProjectileR"
  :keymap projectile-rails-mode-map)

(defadvice projectile-on (after projectile-rails-turn-on activate)
  "Run `projectile-rails-mode' if this is a rails project."
  (when (projectile-rails-root)
    (projectile-rails-mode)
    (run-hooks 'projectile-rails-mode-hook)))

(define-derived-mode projectile-rails-compilation-mode compilation-mode "Projectile Rails Compilation"
  "Compilation mode for projectile-rails output of rails generate."
  (setq-local compilation-error-regexp-alist
	      (cons 'projectile-rails-generate compilation-error-regexp-alist))
  (setq-local compilation-error-regexp-alist-alist
	      (cons '(projectile-rails-generate projectile-rails-errors-regex 1 2)
		    compilation-error-regexp-alist-alist)))

(provide 'projectile-rails)
