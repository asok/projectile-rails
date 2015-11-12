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
               (projectile-rails-sanitize-name "\"name\"")))
 (desc "projectile-rails-declassify"
       (expect "user"
               (projectile-rails-declassify "user"))
       (expect "memberships"
               (projectile-rails-declassify "Memberships"))
       (expect "users_controller"
               (projectile-rails-declassify "UsersController"))
       (expect "admin/users_controller"
               (projectile-rails-declassify "Admin::UsersController"))
       (expect "users/index.html.erb"
               (projectile-rails-declassify "users/index.html.erb")))
 (desc "projectile-rails-hash-keys"
       (expect '("baz" "bar" "foo")
               (let ((hash (make-hash-table :test 'equal)))
                 (puthash "foo" 1 hash)
                 (puthash "bar" 1 hash)
                 (puthash "baz" 1 hash)
                 (projectile-rails-hash-keys hash)))))
