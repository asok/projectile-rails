(expectations
 (desc "projectile-rails-sanitize-name"
       (expect "name"
	       (projectile-rails-sanitize-name ":name"))
       (expect "name"
	       (projectile-rails-sanitize-name "/name"))
       (expect "path/name"
	       (projectile-rails-sanitize-name "/path/name"))
       (expect "name"
	       (projectile-rails-sanitize-name "'name'"))
       (expect "name"
	       (projectile-rails-sanitize-name "\"name\""))
       )
 (desc "projectile-rails-declassify"
       (expect "user"
	       (projectile-rails-declassify "user"))
       (expect "user"
	       (projectile-rails-declassify "users"))
       (expect "memberships"
	       (projectile-rails-declassify "Memberships"))
       (expect "users_controller"
	       (projectile-rails-declassify "UsersController"))
       (expect "admin/users_controller"
       	       (projectile-rails-declassify "Admin::UsersController"))
       )
 )
