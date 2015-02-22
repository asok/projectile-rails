Feature: Finding a test file

Scenario: Finding a controller test
  Given I open the app file "app/models/user.rb"
  And file "test/controllers/bars_controller_test.rb" exists
  And I turn on projectile-mode
  When I run command "projectile-rails-find-test" selecting "controllers/bars_controller"
  Then I am in file "test/controllers/bars_controller_test.rb"
