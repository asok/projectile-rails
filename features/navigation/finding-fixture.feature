Feature: Finding a fixture file

Scenario: Finding the users fixtures
  Given I open the app file "app/models/user.rb"
  And file "test/fixtures/users.yml" exists
  And I turn on projectile-mode
  When I run command "projectile-rails-find-fixture" selecting "users"
  Then I am in file "test/fixtures/users.yml"
