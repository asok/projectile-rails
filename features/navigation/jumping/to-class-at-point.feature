Feature: Jumping to class at point
  In order to do find fast classes at point
  As a user
  I want to be able to run projectile-rails command and jump to the class at point

  Background:
    Given I open the app file "app/controllers/foos_controller.rb"
    And I turn on projectile-mode

  Scenario: Jumping to a model from a ruby symbol in singular form
    And I clear the buffer and insert:
    """
    belongs_to :user
    """
    And I place the cursor between "belongs_to :u" and "ser"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "app/models/user.rb"

  Scenario: Jumping to a model from a ruby symbol in plural form
    And I clear the buffer and insert:
    """
    belongs_to :users
    """
    And I place the cursor between "belongs_to :u" and "sers"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "app/models/user.rb"

  Scenario: Jumping to a lib from a ruby constant in plural form
    And I clear the buffer and insert:
    """
    def bar
      Admin::Memberships.object_id
    end
    """
    And I place the cursor between "Admin::M" and "emberships"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "lib/admin/memberships.rb"

  Scenario: Jumping to a model from a ruby constant
    And I clear the buffer and insert:
    """
    def bar
      User.count
    end
    """
    And I place the cursor between "Use" and "r"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "app/models/user.rb"

  Scenario: Jumping to a model from a namespaced ruby constant
    And I clear the buffer and insert:
    """
    def bar
      Admin::User.count
    end
    """
    And I place the cursor between "Use" and "r"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "app/models/admin/user.rb"

  Scenario: Jumping to a lib from a namespaced ruby constant
    And I clear the buffer and insert:
    """
    def bar
      Admin::Logging.object_id
    end
    """
    And I place the cursor between "Admin::Log" and "ging"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "lib/admin/logging.rb"
    
  Scenario: Not jumping to non-existant model
    And I clear the buffer and insert:
    """
    def bar
      Bar.count
    end
    """
    And I place the cursor between "Ba" and "r"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "app/controllers/foos_controller.rb"
