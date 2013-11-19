Feature: Running rails console
  In order to use a rails console
  As a user
  I want to be able to run it insinde Emacs

Scenario: Running rails console
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run "projectile-rails-console"
  Then I should be in buffer "*ruby*"
  And projectile-rails should be turned on
  When I type "1 + 1"
  And I open the app file "app/models/user.rb"
  When I run "projectile-rails-console"
  Then I should be in buffer "*ruby*"
  And I should see "1 + 1"
