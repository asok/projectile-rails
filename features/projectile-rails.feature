Feature: Enabling projectile-rails mode
  In order to do use the mode
  As a user
  I want to be able to turn it on

  Scenario: Turning on alongside with projectile
    Given I open the app file "app/models/user.rb"
    And I turn on projectile-mode
    Then projectile-rails should be turned on

  Scenario: Not turning on if project is not a rails project
    Given I open the file ".cask"
    And I turn on projectile-mode
    Then projectile-rails should not be turned on
