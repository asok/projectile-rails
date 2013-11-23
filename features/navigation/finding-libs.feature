Feature: Finding libs
  In order to do easly find rails resources
  As a user
  I want to be able to find libs

  Background:
    Given I open the app file "app/models/user.rb"
    And I turn on projectile-mode

  Scenario: Finding libs
    When I run command "projectile-rails-libs" selecting "logging.rb"
    Then I am in file "lib/logging.rb"

  Scenario: Seeing correct libs
    And I am using a test completion system
    When I run "projectile-rails-libs"
    Then the completions should be:
    """
    admin/logging.rb
    admin/memberships.rb
    logging.rb
    """
