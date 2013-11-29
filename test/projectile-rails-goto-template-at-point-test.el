(expectations
 (desc "projectile-rails-template-name"
       (expect "index"
               (projectile-rails-template-name "index"))
       (expect "index"
               (projectile-rails-template-name "index.html"))
       (expect "index"
               (projectile-rails-template-name "index.html.erb"))
       (expect "_index"
               (projectile-rails-template-name "_index.html.erb"))
       (expect "index"
               (projectile-rails-template-name "users/index"))
       (expect "index"
               (projectile-rails-template-name "users/index.html"))
       (expect "index"
               (projectile-rails-template-name "users/index.html.erb"))
       (expect "index"
               (projectile-rails-template-name "admin/users/index"))
       (expect "index"
               (projectile-rails-template-name "admin/users/index.html"))
       (expect "index"
               (projectile-rails-template-name "admin/users/index.html.erb"))

       )
 (desc "projectile-rails-template-format"
       (desc "string at point specifies format"
	     (expect "html"
		     (projectile-rails-template-format "users/index.html.erb"))
	     (expect "js"
		     (projectile-rails-template-format "users/index.js.erb")))
       (desc "string at point does not specify format"
	     (expect "js"
		     (with-mock
		      (stub buffer-file-name => "new.js.erb")
		      (projectile-rails-template-format "users/index")))
	     (desc "formats option specified at current line"
	     	   (expect "json"
	     		   (with-mock
	     		    (stub projectile-rails-current-line => "render 'users/index', formats: [:json]")
	     		    (projectile-rails-template-format "users/index")))
		   (expect "json"
	     		   (with-mock
	     		    (stub projectile-rails-current-line => "render 'users/index', formats: ['json']")
	     		    (projectile-rails-template-format "users/index")))
		   (expect "json"
	     		   (with-mock
	     		    (stub projectile-rails-current-line => "render 'users/index', formats: [\"json\"]")
	     		    (projectile-rails-template-format "users/index")))
		   (expect "json"
	     		   (with-mock
	     		    (stub projectile-rails-current-line => "render 'users/index', :formats => [:json]")
	     		    (projectile-rails-template-format "users/index")))
		   (expect "json"
	     		   (with-mock
	     		    (stub projectile-rails-current-line => "render 'users/index', :formats => ['json']")
	     		    (projectile-rails-template-format "users/index")))
		   (expect "json"
	     		   (with-mock
	     		    (stub projectile-rails-current-line => "render 'users/index', :formats => [\"json\"]")
	     		    (projectile-rails-template-format "users/index")))
		   )
	     )
       )

 (desc "projectile-rails-template-dir"
       (desc "string at point specifies a relative path"
	     (expect "/path/to/project/app/views/admin/users/"
		     (with-mock
		      (stub projectile-project-root => "/path/to/project/")
		      (projectile-rails-template-dir "admin/users/index"))))
       )
       (desc "string at point does not specify a relative path"
	     (desc "calling from a controller"
	     	   (expect "/path/to/project/app/views/admin/users/"
	     		   (with-mock
	     		    (stub buffer-file-name => "/path/to/project/app/controllers/admin/users_controller.rb")
	     		    (stub projectile-project-root => "/path/to/project/")
	     		    (projectile-rails-template-dir "index"))))
	     (desc "not calling from a controller"
	     	   (expect "/path/to/project/app/views/admin/users/"
	     	   	   (let ((default-directory "/path/to/project/app/views/admin/users/"))
	     	   	     (with-mock
	     	   	      (stub buffer-file-name => "/path/to/project/app/views/admin/users/index.html.erb")
	     	   	      (stub projectile-project-root => "/path/to/project/")
	     	   	      (projectile-rails-template-dir "index"))))
	     	   )

	     )
       )
