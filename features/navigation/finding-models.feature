Feature: Finding models
  In order to do easly find rails resources
  As a user
  I want to be able to find models
  
  Background:
    Given I open the app file "app/models/user.rb"
    And I turn on projectile-mode

  Scenario: Switching to a model
    When I run command "projectile-rails-models" selecting "group.rb"
    Then I am in file "app/models/group.rb"

  Scenario: Seeing correct model
    And I am using a test completion system
    When I run "projectile-rails-models"
    Then the completions should be:
    """
    admin/user.rb
    group.rb
    user.rb
    """
