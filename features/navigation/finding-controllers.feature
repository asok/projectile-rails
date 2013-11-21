Feature: Finding controllers
  In order to do easly find rails resources
  As a user
  I want to be able to find controllers
  
  Background:
    Given I open the app file "app/models/user.rb"
    And I turn on projectile-mode

  Scenario: Switching to a controller
    When I run command "projectile-rails-controllers" selecting "groups_controller.rb"
    Then I am in file "app/controllers/groups_controller.rb"

  Scenario: Seeing correct controllers
    And I am using a test completion system
    When I run "projectile-rails-controllers"
    Then the completions should be:
    """
    admin/users_controller.rb
    application_controller.rb
    groups_controller.rb
    users_controller.rb
    """
