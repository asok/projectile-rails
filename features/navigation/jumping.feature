Feature: Jumping to file at point
  In order to do find fast things at point
  As a user
  I want to be able to run projectile-rails command and jump to the file at point

  Background:
    Given I open the app file "app/controllers/foos_controller.rb"
    And I turn on projectile-mode

  Scenario: Jumping to a model from a ruby symbol in singular form
    And I insert:
    """
    belongs_to :user
    """
    And I place the cursor between "belongs_to :u" and "ser"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "app/models/user.rb"

  Scenario: Jumping to a model from a ruby symbol in plural form
    And I exit the snippets
    And I insert:
    """
    belongs_to :users
    """
    And I place the cursor between "belongs_to :u" and "sers"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "app/models/user.rb"

  Scenario: Jumping to a model from a ruby constant
    And I insert:
    """
    def bar
      User.count
    end
    """
    And I place the cursor between "Use" and "r"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "app/models/user.rb"

  Scenario: Jumping to a model from a namespaced ruby constant
    And I insert:
    """
    def bar
      Admin::User.count
    end
    """
    And I place the cursor between "Use" and "r"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "app/models/admin/user.rb"

  Scenario: Jumping to a lib from a namespaced ruby constant
    And I insert:
    """
    def bar
      Admin::Logging.object_id
    end
    """
    And I place the cursor between "Admin::Log" and "ging"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "lib/admin/logging.rb"
    
  Scenario: Not jumping to non-existant model
    And I insert:
    """
    def bar
      Bar.count
    end
    """
    And I place the cursor between "Ba" and "r"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "app/controllers/foos_controller.rb"
