Feature: Creating new files
  
Scenario: Creating a new model
  Given file "app/models/user.rb" exists
  And I open the app file "app/models/foo.rb"
  And I turn on projectile-mode
  When I run command "projectile-rails-find-model" selecting "bar" and confirm
  Then I am in file "app/models/bar.rb"
