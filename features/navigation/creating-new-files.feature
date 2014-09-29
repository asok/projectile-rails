Feature: Creating new files
  
Scenario: Creating a new model
  Given file "app/models/user.rb" exists
  And I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run command "projectile-rails-find-model" selecting "foo" and confirm
  Then I am in file "app/models/foo.rb"

Scenario: Creating a new controller
  Given file "app/models/user.rb" exists
  And I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run command "projectile-rails-find-controller" selecting "foos" and confirm
  Then I am in file "app/controllers/foos_controller.rb"

Scenario: Creating a new template
  Given file "app/models/user.rb" exists
  And I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run command "projectile-rails-find-view" selecting "users/foo.html.erb" and confirm
  Then I am in file "app/views/users/foo.html.erb"
