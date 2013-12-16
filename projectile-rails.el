;;; projectile-rails.el --- Minor mode for Rails projects based on projectile-mode

;; Copyright (C) 2013 Adam Sokolnicki

;; Author:            Adam Sokolnicki <adam.sokolnicki@gmail.com>
;; URL:               https://github.com/asok/projectile-rails
;; Version:           0.4.0
;; Keywords:          rails, projectile
;; Package-Requires:  ((projectile "1.0.0-cvs") (inflections "1.1") (inf-ruby "2.2.6"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; To make it start alongside projectile-mode:
;;
;;    (add-hook 'projectile-mode-hook 'projectile-rails-on)
;;
;;; Code:

(require 'projectile)
(require 'inf-ruby)
(require 'inflections)

(defgroup projectile-rails nil
  "Rails mode based on projectile"
  :prefix "projectile-rails-"
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

(defcustom projectile-rails-errors-re
  "\\([0-9A-Za-z@_./\:-]+\\.rb\\):?\\([0-9]+\\)?"
  "The regex used to find errors with file paths."
  :group 'projectile-rails
  :type 'string)

(defcustom projectile-rails-generate-filepath-re
  "^\\s-+\\(?:create\\|exists\\|conflict\\|skip\\)\\s-+\\(.+\\)$"
  "The regex used to find file paths in `projectile-rails-generate-mode'."
  :group 'projectile-rails
  :type 'string)

(defcustom projectile-rails-javascript-dirs
  '("app/assets/javascripts/" "lib/assets/javascripts/" "public/javascripts/")
  "The list directories to for javascript files in."
  :group 'projectile-rails
  :type '(repeat stirng))

(defcustom projectile-rails-expand-snippet t
  "If not nil newly created buffers will be pre-filled with class skeleton.")

(defcustom projectile-rails-add-keywords t
  "If not nil the rails keywords will be font locked in the mode's bufffers.")

(defcustom projectile-rails-keymap-prefix (kbd "C-c r")
  "`projectile-rails-mode' keymap prefix."
  :group 'projectile-rails
  :type 'string)

(defmacro projectile-rails-if-zeus (command-for-zeus command-for-bundler)
  `(if (file-exists-p (projectile-expand-root ".zeus.sock"))
       ,command-for-zeus
     ,command-for-bundler))

(defmacro projetile-rails-with-root (body-form)
  `(let ((default-directory (projectile-rails-root)))
     ,body-form))

(defmacro projectile-rails-find-current-resource (dir re)
  "RE will be the argument to `s-lex-format'.

The binded variables are \"singular\" and \"plural\"."
  `(let* ((singular (projectile-rails-current-resource-name))
	  (plural (pluralize-string singular))
	  (files (--filter
		  (string-match-p (s-lex-format ,re) it)
		  (projectile-dir-files (projectile-expand-root ,dir)))))
     (projectile-rails-goto-file
       (if (= (length files) 1)
	   (-first-item files)
	 (projectile-completing-read "Which exactly: " files)))))

(defun projectile-rails--highlight-keywords (keywords)
  "Highlight the passed KEYWORDS in current buffer."
  (font-lock-add-keywords
   nil
   (list (list
          (concat "\\(^\\|[^_:.@$]\\|\\.\\.\\)\\b"
                  (regexp-opt keywords t)
		  "\\_>")
          (list 2 projectile-rails-font-lock-face-name)))))

(defun projectile-rails-add-keywords-for-file-type ()
  "Apply extra font lock keywords specific to models, controllers etc."
  (loop for (re keywords) in `(("_controller\\.rb$"   ,projectile-rails-controller-keywords)
                               ("app/models/.+\\.rb$" ,projectile-rails-model-keywords)
                               ("db/migrate/.+\\.rb$" ,projectile-rails-migration-keywords))
        do (when (and (buffer-file-name) (string-match-p re (buffer-file-name)))
             (projectile-rails--highlight-keywords
	      (append keywords projectile-rails-active-support-keywords)))))

(defun projectile-rails-dir-files (dirs)
  "Similar to `projectile-dir-files' except the argument is a list of directories to look in."
  (-flatten (--map (projectile-dir-files (projectile-expand-root it)) dirs)))

(defun projectile-rails-choices (dirs)
  "Uses `projectile-rails-dir-files' function to find files in directories.

The DIRS is list of lists consisting of a directory path and regexp to filter files from that directory.
Returns a hash table with keys being short names and values being relative paths to the files."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (dir re) in dirs do
	  (loop for file in (projectile-dir-files (projectile-expand-root dir)) do
		(when (string-match re file)
		  (puthash (match-string 1 file) file hash))))
    hash))

(defun projectile-rails-hash-keys (hash)
  (let (keys)
    (maphash (lambda (key value) (setq keys (cons key keys))) hash)
    keys))

(defun projectile-rails-find-resource (prompt dirs)
  (let ((choices (projectile-rails-choices dirs)))
     (projectile-rails-goto-file
      (gethash (projectile-completing-read prompt (projectile-rails-hash-keys choices)) choices))))

(defun projectile-rails-find-model ()
  (interactive)
  (projectile-rails-find-resource "model: " '(("app/models/" "/\\(.+\\)\\.rb$"))))

(defun projectile-rails-find-controller ()
  (interactive)
  (projectile-rails-find-resource "controller: " '(("app/controllers/" "/\\(.+\\)_controller\\.rb$"))))

(defun projectile-rails-find-view ()
  (interactive)
  (projectile-rails-find-resource
   "view: "
   `(("app/views/" ,(concat "app/views/\\(.+\\)" projectile-rails-views-re)))))

(defun projectile-rails-find-helper ()
  (interactive)
  (projectile-rails-find-resource "helper: " '(("app/helpers/" "/\\(.+\\)_helper\\.rb$"))))

(defun projectile-rails-find-lib ()
  (interactive)
  (projectile-rails-find-resource "lib: " '(("lib/" "/\\(.+\\)\\.rb$"))))

(defun projectile-rails-find-spec ()
  (interactive)
  (projectile-rails-find-resource "spec: " '(("spec/" "/\\(.+\\)_spec\\.rb$"))))

(defun projectile-rails-find-migration ()
  (interactive)
  (projectile-rails-find-resource "migration: " '(("db/migrate/" "/\\(.+\\)\\.rb$"))))

(defun projectile-rails-find-javascript ()
  (interactive)
  (projectile-rails-find-resource
   "stylesheet: "
   (--map (list it "/\\(.+\\)\\.[^.]+$") projectile-rails-javascript-dirs)))

(defun projectile-rails-find-initializer ()
  (interactive)
  (projectile-rails-find-resource "initializer: " '(("config/initializers/" "/\\(.+\\)\\.rb$"))))

(defun projectile-rails-find-environment ()
  (interactive)
  (projectile-rails-find-resource
   "environment: "
   '(("config/" "/\\(application\\|environment\\)\\.rb$")
     ("config/environments/" "\\(.+\\)\\.rb$"))))

(defun projectile-rails-find-locale ()
  (interactive)
  (projectile-rails-find-resource "locale: " '(("config/locales/" "/\\(.+\\)\\.\\(?:rb\\|yml\\)$"))))

(defun projectile-rails-find-current-model ()
  (interactive)
  (projectile-rails-find-current-resource
   "app/models/" "/${singular}\\.rb$"))

(defun projectile-rails-find-current-controller ()
  (interactive)
  (projectile-rails-find-current-resource
   "app/controllers/" "/${plural}_controller\\.rb$"))

(defun projectile-rails-find-current-view ()
  (interactive)
  (projectile-rails-find-current-resource
   "app/views/" "/${plural}/.+$"))

(defun projectile-rails-find-current-helper ()
  (interactive)
  (projectile-rails-find-current-resource
   "app/helpers/" "/${plural}_helper\\.rb$"))

(defun projectile-rails-find-current-spec ()
  (interactive)
  (if (fboundp 'rspec-toggle-spec-and-target)
      (rspec-toggle-spec-and-target)
    (projectile-find-test-file)))

(defun projectile-rails-find-current-migration ()
  (interactive)
  (projectile-rails-find-current-resource
   "db/migrate/" "/[0-9]\\{14\\}.*_\\(${plural}\\|${singular}\\).*\\.rb$"))

(defun projectile-rails-current-resource-name ()
  "Returns a resource name extracted from the name of the currently visiting file"
  (let ((file-name (buffer-file-name)))
    (if file-name
	(singularize-string
	 (loop for re in '("app/models/\\(.+\\)\\.rb$"
			   "app/controllers/\\(.+\\)_controller\\.rb$"
			   "app/views/\\(.+\\)/[^/]+$"
			   "app/helpers/\\(.+\\)_helper\\.rb$"
			   "spec/.*/\\([a-z_]+?\\)\\(_controller\\)?_spec\\.rb$")
	       until (string-match re file-name)
	       finally return (match-string 1 file-name))))
    )
  )

(defun projectile-rails-list-entries (fun dir)
  (--map
   (substring it (length (concat (projectile-rails-root) dir)))
   (funcall fun (projectile-expand-root dir))))

(defun projectile-rails-find-log ()
  (interactive)
  ;;logs tend to not be under scm so do not resort to projectile-dir-files
  (find-file (projectile-expand-root
	      (concat
	       "log/"
	       (projectile-completing-read
		"log: "
		(projectile-rails-list-entries 'f-files "log/")))))
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
     (projetile-rails-with-root
      (shell-command-to-string
       (projectile-rails-if-zeus "zeus rake -T" "bundle exec rake -T"))))))

(defun projectile-rails-rake (task)
  (interactive
   (list
    (projectile-completing-read
     "Rake (default: default): "
     (projectile-rails-pcmpl-rake-tasks))))
  (let ((default-directory (projectile-rails-root)))
    (projetile-rails-with-root
     (compile
      (concat
       (projectile-rails-if-zeus "zeus rake " "bundle exec rake ") (if (= 0 (length task))
                                                          "default"
                                                        task))
      'projectile-rails-compilation-mode))))

(defun projectile-rails-root ()
  "Returns rails root directory if this file is a part of a Rails application else nil"
  (and
   (projectile-project-p)
   (file-exists-p (projectile-expand-root "config/environment.rb"))
   (projectile-project-root)))

(defun projectile-rails-console ()
  (interactive)
  (projetile-rails-with-root
   (with-current-buffer (run-ruby
			 (projectile-rails-if-zeus "zeus console" "bundle exec rails console"))
     (projectile-rails-mode +1))))

(defun projectile-rails-expand-snippet-maybe ()
  (when (and (fboundp 'yas-expand-snippet)
  	     (and (buffer-file-name) (not (file-exists-p (buffer-file-name))))
	     (s-blank? (buffer-string))
  	(projectile-rails-expand-corresponding-snippet))))

(defun projectile-rails-expand-corresponding-snippet ()
  (let ((name (buffer-file-name)))
    (yas-expand-snippet
     (cond ((string-match "app/controllers/\\(.+\\)\\.rb$" name)
	    (format
	     "class %s < ${1:ApplicationController}\n$2\nend"
	     (s-join "::" (projectile-rails-classify (match-string 1 name)))))
	   ((string-match "spec/[^/]+/\\(.+\\)_spec\\.rb$" name)
	    (format
	     "require \"spec_helper\"\n\ndescribe %s do\n$1\nend"
	     (s-join "::" (projectile-rails-classify (match-string 1 name)))))
	   ((string-match "app/models/\\(.+\\)\\.rb$" name)
	    (format
	     "class %s < ${1:ActiveRecord::Base}\n$2\nend"
	     (s-join "::" (projectile-rails-classify (match-string 1 name)))))
	   ((string-match "lib/\\(.+\\)\\.rb$" name)
	    (let ((parts (projectile-rails-classify (match-string 1 name))))
	      (format
	       (concat
		(s-join
		 ""
		 (--map (s-lex-format "module ${it}\n") (butlast parts)))
		"${1:module} %s\n$2\nend"
		(s-join "" (make-list (1- (length parts)) "\nend")))
	       (-last-item parts))))))))

(defun projectile-rails-classify (name)
  "Accepts a filepath, splits it by '/' character and classifieses each of the element"
  (--map (replace-regexp-in-string "_" "" (upcase-initials it)) (split-string name "/")))

(defun projectile-rails-declassify (name)
  "Converts passed string to a relative filepath."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      "::" "/"
      (replace-regexp-in-string
       " " "_"
       (replace-regexp-in-string
	"\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" name))))))

(defun projectile-rails-generate ()
  "Runs rails generate command"
  (interactive)
  (projetile-rails-with-root
   (let ((command-prefix (projectile-rails-if-zeus
                          "zeus generate "
                          "bundle exec rails generate ")))
     (compile
      (concat command-prefix (read-string command-prefix))
      'projectile-rails-generate-mode))))

(defun projectile-rails-sanitize-and-goto-file (dir name &optional ext)
  "Calls `projectile-rails-goto-file' with passed arguments sanitizing them before."
  (projectile-rails-goto-file
   (concat 
    (projectile-rails-sanitize-dir-name dir) (projectile-rails-declassify name) ext)))

(defun projectile-rails-goto-file (filepath)
  "Finds the FILEPATH after expanding root."
  (projectile-rails-ff (projectile-expand-root filepath)))

(defun projectile-rails-goto-gem (gem)
  "Uses `bundle-open' to open GEM. If the function is not defined notifies user."
  (if (not (fboundp 'bundle-open))
      (user-error "Please install budler.el from https://github.com/tobiassvn/bundler.el")
    (message "Using bundle-open command to open the gem")
    (bundle-open (car (s-split "/" gem))))
  )

(defun projectile-rails-goto-javascript-at-point ()
  (let ((name (thing-at-point 'filename)))
    (projectile-rails-ff
     (loop for dir in projectile-rails-javascript-dirs
	   for re = (s-lex-format "${dir}${name}\\..+$")
	   for files = (projectile-rails-dir-files (list dir))
	   for file = (--first (string-match-p re it) files)
	   until file
	   finally return (projectile-expand-root file))))
  )

(defun projectile-rails-goto-file-at-point ()
  "Tries to find file at point"
  (interactive)
  (let ((name (projectile-rails-name-at-point))
	(line (projectile-rails-current-line))
	(case-fold-search nil))
    (cond ((string-match "Processing by \\(.+\\)#\\(?:[^ ]+\\)" line)
	   (projectile-rails-sanitize-and-goto-file
	    "app/controllers/" (match-string 1 line) ".rb"))
	  
	  ((string-match "Rendered \\([^ ]+\\)" line)
	   (projectile-rails-sanitize-and-goto-file
	    "app/views/" (match-string 1 line)))
	  
	  ((string-match-p "\\_<render\\_>" line)
	   (projectile-rails-goto-template-at-point))

	  ((string-match-p "^//= require .+$" line)
	   (projectile-rails-goto-javascript-at-point))

	  ((string-match-p "\\_<require_relative\\_>" line)
	   (projectile-rails-ff (expand-file-name (concat (thing-at-point 'filename) ".rb"))))

	  ((string-match-p "\\_<require\\_>" line)
	   (projectile-rails-goto-gem (thing-at-point 'filename)))
	  
	  ((not (string-match-p "^[A-Z]" name))
	   (projectile-rails-sanitize-and-goto-file "app/models/" (singularize-string name) ".rb"))

	  ((not (string-match-p "^[A-Z]" name))
	   (projectile-rails-sanitize-and-goto-file "app/models/" (singularize-string name) ".rb"))

	  ((string-match-p "^[A-Z]" name)
	   (loop for dir in (-concat
			     (--map
			      (concat "app/" it)
			      (projectile-rails-list-entries 'f-directories "app/"))
			     '("lib/"))
		 until (projectile-rails-sanitize-and-goto-file dir name ".rb"))))
    )
  )

(defun projectile-rails-template-name (template)
  (-first-item (s-split "\\." (-last-item (s-split "/" template)))))

(defun projectile-rails-template-format (template)
  (let ((at-point-re "\\.\\([^.]+\\)\\.[^.]+$")
	(at-line-re "formats\\(?:'\"\\|:\\)?\\s-*\\(?:=>\\)?\\s-*\\[[:'\"]\\([a-zA-Z0-9]+\\)['\"]?\\]"))
    (cond ((string-match at-point-re template)
	   (match-string 1 template))
	  ((string-match at-line-re (projectile-rails-current-line))
	   (match-string 1 (projectile-rails-current-line)))
	  (t
	   (when (string-match at-point-re (buffer-file-name))
	     (match-string 1 (buffer-file-name)))))))

(defun projectile-rails-template-dir (template)
  (projectile-rails-sanitize-dir-name
   (cond ((string-match "\\(.+\\)/[^/]+$" template)
	  (projectile-expand-root
	   (concat "app/views/" (match-string 1 template))))
	 ((string-match "app/controllers/\\(.+\\)_controller\\.rb$" (buffer-file-name))
	  (projectile-expand-root
	   (concat "app/views/" (match-string 1 (buffer-file-name)))))
	 (t
	  default-directory))))

(defun projectile-rails-goto-template-at-point ()
  (interactive)
  (let* ((template (projectile-rails-name-at-point))
	 (dir (projectile-rails-template-dir template))
	 (name (projectile-rails-template-name template))
	 (format (projectile-rails-template-format template)))
    (if format
	(loop for processor in '("erb" "haml" "slim")
	      for template = (s-lex-format "${dir}${name}.${format}.${processor}")
	      for partial = (s-lex-format "${dir}_${name}.${format}.${processor}")
	      until (or
		     (projectile-rails-ff template)
		     (projectile-rails-ff partial)))
      (message "Could not recognize the template's format")
      (dired dir))))

(defun projectile-rails-goto-gemfile ()
  (interactive)
  (projectile-rails-goto-file "Gemfile"))

(defun projectile-rails-goto-schema ()
  (interactive)
  (projectile-rails-goto-file "db/schema.rb"))

(defun projectile-rails-goto-routes ()
  (interactive)
  (projectile-rails-goto-file "config/routes.rb"))

(defun projectile-rails-goto-spec-helper ()
  (interactive)
  (projectile-rails-goto-file "spec/spec_helper.rb"))

(defun projectile-rails-ff (path &optional ask)
  "Calls `find-file' function on PATH when it is not nil and the file exists.

If file does not exist and ASK in not nil it will ask user to proceed."
  (if (or (and path (file-exists-p path))
	  (and ask (yes-or-no-p (s-lex-format "File does not exists. Create a new buffer ${path} ?"))))
      (find-file path)))

(defun projectile-rails-name-at-point ()
  (projectile-rails-sanitize-name (symbol-name (symbol-at-point))))

(defun projectile-rails-apply-ansi-color ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(defun projectile-rails-make-buttons (buffer exit-code)
  (with-current-buffer buffer
    (goto-char 0)
    (while (re-search-forward projectile-rails-generate-filepath-re (max-char) t)
      (let ((beg (match-beginning 1))
	    (end (match-end 1)))
	(when (file-exists-p (projectile-expand-root (buffer-substring-no-properties beg end)))
	  (make-button beg end 'action 'projectile-rails-generate-ff 'follow-link t))))
    )
  )

(defun projectile-rails-generate-ff (button)
  (find-file (projectile-expand-root (button-label button))))

(defun projectile-rails-sanitize-name (name)
  (cond ((or (s-starts-with? ":" name) (s-starts-with? "/" name))
	 (substring name 1))
	((or
	  (and (s-starts-with? "'" name) (s-ends-with? "'" name))
	  (and (s-starts-with? "\"" name) (s-ends-with? "\"" name)))
	 (substring name 1 (1- (length name))))
	(t
	 name)))

(defun projectile-rails-sanitize-dir-name (name)
  (if (s-ends-with? "/" name) name (concat name "/")))

(defun projectile-rails-current-line ()
  (save-excursion
    (let (beg)
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (buffer-substring-no-properties beg (point)))))

(defvar projectile-rails-mode-goto-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'projectile-rails-find-current-model)
    (define-key map (kbd "c") 'projectile-rails-find-current-controller)
    (define-key map (kbd "v") 'projectile-rails-find-current-view)
    (define-key map (kbd "h") 'projectile-rails-find-current-helper)
    (define-key map (kbd "s") 'projectile-rails-find-current-spec)
    (define-key map (kbd "n") 'projectile-rails-find-current-migration)
    (define-key map (kbd "f") 'projectile-rails-goto-file-at-point)
    (define-key map (kbd "g") 'projectile-rails-goto-gemfile)
    (define-key map (kbd "r") 'projectile-rails-goto-routes)
    (define-key map (kbd "h") 'projectile-rails-goto-schema)
    (define-key map (kbd "p") 'projectile-rails-goto-spec-helper)
    map)
  "A goto map for `projectile-rails-mode'."
)

(defvar projectile-rails-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "m") 'projectile-rails-find-model)
      (define-key prefix-map (kbd "M") 'projectile-rails-find-current-model)
      (define-key prefix-map (kbd "c") 'projectile-rails-find-controller)
      (define-key prefix-map (kbd "C") 'projectile-rails-find-current-controller)
      (define-key prefix-map (kbd "v") 'projectile-rails-find-view)
      (define-key prefix-map (kbd "V") 'projectile-rails-find-current-view)
      (define-key prefix-map (kbd "h") 'projectile-rails-find-helper)
      (define-key prefix-map (kbd "H") 'projectile-rails-find-current-helper)
      (define-key prefix-map (kbd "l") 'projectile-rails-find-lib)
      (define-key prefix-map (kbd "s") 'projectile-rails-find-spec)
      (define-key prefix-map (kbd "S") 'projectile-rails-find-current-spec)
      (define-key prefix-map (kbd "n") 'projectile-rails-find-migration)
      (define-key prefix-map (kbd "N") 'projectile-rails-find-current-migration)
      (define-key prefix-map (kbd "i") 'projectile-rails-find-initializer)
      (define-key prefix-map (kbd "j") 'projectile-rails-find-javascript)
      (define-key prefix-map (kbd "o") 'projectile-rails-find-log)
      (define-key prefix-map (kbd "e") 'projectile-rails-find-environment)
      (define-key prefix-map (kbd "a") 'projectile-rails-find-locale)
      (define-key prefix-map (kbd "r") 'projectile-rails-console)
      (define-key prefix-map (kbd "k") 'projectile-rails-rake)
      (define-key prefix-map (kbd "t") 'projectile-rails-generate)
      (define-key prefix-map (kbd "RET") 'projectile-rails-goto-file-at-point)

      (define-key prefix-map (kbd "g") projectile-rails-mode-goto-map)
      (define-key map projectile-rails-keymap-prefix prefix-map))
    map)
  "Keymap for `projectile-rails-mode'.")

(easy-menu-define projectile-rails-menu projectile-rails-mode-map
  "Menu for `projectile-rails-mode'."
  '("Rails"
    ["Find model"		 projectile-rails-find-model]
    ["Find controller"		 projectile-rails-find-controller]
    ["Find view"		 projectile-rails-find-view]
    ["Find helper"		 projectile-rails-find-helper]
    ["Find lib"			 projectile-rails-find-lib]
    ["Find spec"		 projectile-rails-find-spec]
    ["Find log"                  projectile-rails-find-log]
    ["Find migration"            projectile-rails-find-migration]
    ["Find javascript"           projectile-rails-find-javascript]
    ["Find initializer"          projectile-rails-find-initializer]
    ["Find environment"          projectile-rails-find-environment]
    ["Find locale"               projectile-rails-find-locale]
    "--"
    ["Go to file at point"	 projectile-rails-goto-file-at-point]
    "--"
    ["Go to Gemfile"	         projectile-rails-goto-gemfile]
    ["Go to routes"	         projectile-rails-goto-routes]
    ["Go to schema"	         projectile-rails-goto-schema]
    ["Go to spec_helper"	 projectile-rails-goto-spec-helper]
    "--"
    ["Go to current model"	 projectile-rails-find-current-spec]
    ["Go to current controller"	 projectile-rails-find-current-controller]
    ["Go to current view"	 projectile-rails-find-current-view]
    ["Go to current spec"	 projectile-rails-find-current-spec]
    ["Go to current migration"	 projectile-rails-find-current-migration]
    "--"
    ["Run console"		 projectile-rails-console]
    ["Run rake"			 projectile-rails-rake]
    ["Run rails generate"	 projectile-rails-generate]))

;;;###autoload
(define-minor-mode projectile-rails-mode
  "Rails mode based on projectile"
  :init-value nil
  :lighter " Rails"
  (when projectile-rails-mode
    (and projectile-rails-expand-snippet (projectile-rails-expand-snippet-maybe))
    (and projectile-rails-add-keywords (projectile-rails-add-keywords-for-file-type))))

;;;###autoload
(defun projectile-rails-on ()
  "Enable `projectile-rails-mode' minor mode if this is a rails project."
  (when (projectile-rails-root)
    (projectile-rails-mode +1)))

(defun projectile-rails-off ()
  "Disable `projectile-rails-mode' minor mode."
  (projectile-rails-mode -1))

(define-derived-mode projectile-rails-compilation-mode compilation-mode "Projectile Rails Compilation"
  "Compilation mode used by `projectile-rails'."
  (add-hook 'compilation-filter-hook 'projectile-rails-apply-ansi-color nil t))

(define-derived-mode projectile-rails-generate-mode projectile-rails-compilation-mode "Projectile Rails Generate"
  "Mode for output of rails generate."
  (add-hook 'compilation-finish-functions 'projectile-rails-make-buttons nil t))

(provide 'projectile-rails)

;;; projectile-rails.el ends here
