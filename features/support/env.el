(require 'f)

(defvar projectile-rails-support-path
  (f-dirname load-file-name))

(defvar projectile-rails-features-path
  (f-parent projectile-rails-support-path))

(defvar projectile-rails-root-path
  (f-parent projectile-rails-features-path))

(add-to-list 'load-path projectile-rails-root-path)

(defvar projectile-rails-app-path
  (concat projectile-rails-features-path "/app"))

(require 'projectile-rails)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 (require 'projectile-rails)
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
