Feature: Using rails generate command
  In order to generate projects's files
  As a user
  I want to be able to run rails generate command from Emacs
  
Scenario: Runnning generate migrate
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run command "projectile-rails-generate" inputting "rspec:install"
  And I switch to buffer "*projectile-rails-compilation*"
  Then I should see "bundle exec rails generate rspec:install"
