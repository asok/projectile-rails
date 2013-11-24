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
 )
