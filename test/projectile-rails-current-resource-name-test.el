(expectations
 (desc "projectile-rails-current-resource-name"
       (desc "non-namespaced model"
             (expect "user"
                     (with-mock
                      (stub buffer-file-name => "/app/models/user.rb")
                      (projectile-rails-current-resource-name))))
       (desc "namespaced model"
             (expect "user"
                     (with-mock
                      (stub buffer-file-name => "/app/models/foo/bar/user.rb")
                      (projectile-rails-current-resource-name)))))
 (desc "non-namespaced controller"
       (expect "user"
               (with-mock
                (stub buffer-file-name => "/app/controllers/users_controller.rb")
                (projectile-rails-current-resource-name))))
 (desc "namespaced controller"
       (expect "user"
               (with-mock
                (stub buffer-file-name => "/app/controllers/foo/bar/users_controller.rb")
                (projectile-rails-current-resource-name))))
 (desc "non-namespaced template"
       (expect "user"
               (with-mock
                (stub buffer-file-name => "/app/views/users/new.html.erb")
                (projectile-rails-current-resource-name))))
 (desc "namespaced template"
       (expect "user"
               (with-mock
                (stub buffer-file-name => "/app/views/foo/bar/users/new.html.erb")
                (projectile-rails-current-resource-name))))
 (desc "non-namespaced helper"
       (expect "user"
               (with-mock
                (stub buffer-file-name => "/app/helpers/users_helper.rb")
                (projectile-rails-current-resource-name))))
 (desc "namespaced helper"
       (expect "user"
               (with-mock
                (stub buffer-file-name => "/app/helpers/foo/bar/users_helper.rb")
                (projectile-rails-current-resource-name))))
 (desc "non-namespaced model spec"
       (expect "user"
               (with-mock
                (stub buffer-file-name => "/spec/models/user_spec.rb")
                (projectile-rails-current-resource-name))))
 (desc "namespaced model spec"
       (expect "user"
               (with-mock
                (stub buffer-file-name => "/spec/models/foo/bar/user_spec.rb")
                (projectile-rails-current-resource-name))))
 (desc "non-namespaced controller spec"
       (expect "user"
               (with-mock
                (stub buffer-file-name => "/spec/controllers/users_controller_spec.rb")
                (projectile-rails-current-resource-name))))
 (desc "namespaced controller spec"
       (expect "user"
               (with-mock
                (stub buffer-file-name => "/spec/controllers/foo/bar/users_controller_spec.rb")
                (projectile-rails-current-resource-name)))))
