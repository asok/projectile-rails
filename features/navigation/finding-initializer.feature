Feature: Finding an initializer file

Scenario: Finding initializer file
  Given I open the app file "app/models/user.rb"
  And file "config/initializers/bar.rb" exists
  And file "config/initializers/foo.rb" exists
  And I turn on projectile-mode
  When I run command "projectile-rails-find-initializer" selecting "foo"
  Then I am in file "config/initializers/foo.rb"
