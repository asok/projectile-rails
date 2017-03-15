Feature: Running rails console
  In order to use a rails console
  As a user
  I want to be able to run it inside Emacs

Scenario: Running rails console
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-rails-mode
  When I run "projectile-rails-console"
  Then I should be in buffer "*rails*"
  And projectile-rails should be turned on
  When I type "1 + 1"
  And I open the app file "app/models/user.rb"
  And I run "projectile-rails-console"
  Then I should be in buffer "*rails*"
  And I should see "1 + 1"
