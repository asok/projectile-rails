Feature: Finding helpers
  In order to do easly find rails resources
  As a user
  I want to be able to find helpers

  Background:
    Given I open the app file "app/models/user.rb"
    And I turn on projectile-mode

  Scenario: Finding helpers
    When I run command "projectile-rails-find-helper" selecting "users_helper.rb"
    Then I am in file "app/helpers/users_helper.rb"

  Scenario: Seeing correct helpers
    And I am using a test completion system
    When I run "projectile-rails-find-helper"
    Then the completions should be:
    """
    application_helper.rb
    groups_helper.rb
    users_helper.rb
    """
