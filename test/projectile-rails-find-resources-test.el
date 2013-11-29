(expectations
 (desc "projectile-rails-dir-files"
       (expect '("admin/user.rb" "user.rb")
	       (with-mock
		(stub projectile-dir-files => '("/path/app/models/admin/user.rb"
						"/path/app/models/user.rb"))
		(projectile-rails-dir-files "/path/app/models/" "\\.rb$")))

       (expect '("admin/users_controller.rb" "users_controller.rb")
	       (with-mock
		(stub projectile-dir-files => '("/path/app/controllers/admin/users_controller.rb"
						"/path/app/controllers/users_controller.rb"))
		(projectile-rails-dir-files "/path/app/controllers/" "\\.rb$")))

       (expect '("admin/users/index.html.haml" "users/_user.html.erb" "users/index.html.slim")
	       (with-mock
		(stub projectile-dir-files => '("/path/app/views/admin/users/index.html.haml"
						"/path/app/views/users/_user.html.erb"
						"/path/app/views/users/index.html.slim"))
		(projectile-rails-dir-files "/path/app/views/" projectile-rails-views-re)))

       (expect '("application_helper.rb")
	       (with-mock
		(stub projectile-dir-files => '("/path/app/helpers/application_helper.rb"))
		(projectile-rails-dir-files "/path/app/helpers/" "\\.rb$")))

       (expect '("admin/logging.rb")
	       (with-mock
		(stub projectile-dir-files => '("/path/lib/admin/logging.rb"))
		(projectile-rails-dir-files "/path/lib/" "\\.rb$")))

       (expect '("models/user_spec.rb" "lib/logging_spec.rb" "controllers/users_controller_spec.rb")
	       (with-mock
		(stub projectile-dir-files => '("/path/spec/models/user_spec.rb"
						"/path/spec/lib/logging_spec.rb"
						"/path/spec/controllers/users_controller_spec.rb"
						"/path/spec/spec_helper"))
		(projectile-rails-dir-files "/path/spec/" "\\.rb$")))
       )
 )
