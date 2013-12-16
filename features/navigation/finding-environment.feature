Feature: Finding an environment file

Background:
  Given I open the app file "app/models/user.rb"
  And file "config/environments/development.rb" exists
  And file "config/environments/production.rb" exists
  And file "config/environment.rb" exists
  And file "config/application.rb" exists
  And I turn on projectile-mode

Scenario: Finding environment file inside 'config/environments/' directory
  When I run command "projectile-rails-find-environment" selecting "production"
  Then I am in file "config/environments/production.rb"

Scenario: Finding environment "config/application.rb" file
  When I run command "projectile-rails-find-environment" selecting "application"
  Then I am in file "config/application.rb"

Scenario: Finding environment "config/application.rb" file
  When I run command "projectile-rails-find-environment" selecting "environment"
  Then I am in file "config/environment.rb"
