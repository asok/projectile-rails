Feature: Finding specs
  In order to do easly find rails resources
  As a user
  I want to be able to find specs

  Background:
    Given I open the app file "app/models/user.rb"
    And I turn on projectile-mode

  Scenario: Finding specs
    When I run command "projectile-rails-specs" selecting "models/user_spec.rb"
    Then I am in file "spec/models/user_spec.rb"

  Scenario: Seeing correct specs
    And I am using a test completion system
    When I run "projectile-rails-specs"
    Then the completions should be:
    """
    controllers/groups_controller_spec.rb
    controllers/users_controller_spec.rb
    helpers/groups_helper_spec.rb
    helpers/users_helper_spec.rb
    lib/logging_spec.rb
    models/group_spec.rb
    models/user_spec.rb
    """
