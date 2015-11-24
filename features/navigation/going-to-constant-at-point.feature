Feature: Going to constant at point
  In order to do find fast constants at point
  As a user
  I want to be able to run projectile-rails command and jump to the constant at point

  Background:
    Given I open the app file "app/controllers/foos_controller.rb"
    And I turn on projectile-mode

  Scenario: Going to a model from a ruby symbol in singular form
    And file "app/models/user.rb" exists
    And I clear the buffer and insert:
    """
    belongs_to :user
    """
    And I place the cursor between "belongs_to :u" and "ser"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/models/user.rb"

  Scenario: Going to a model from a ruby symbol in plural form
    And file "app/models/user.rb" exists
    And I clear the buffer and insert:
    """
    belongs_to :users
    """
    And I place the cursor between "belongs_to :u" and "sers"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/models/user.rb"

  Scenario: Going to a lib from a ruby constant in plural form
    And file "lib/admin/memberships.rb" exists
    And I clear the buffer and insert:
    """
    def bar
      Admin::Memberships.object_id
    end
    """
    And I place the cursor between "Admin::M" and "emberships"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "lib/admin/memberships.rb"

  Scenario: Going to a model from a ruby constant
    And file "app/models/user.rb" exists
    And I clear the buffer and insert:
    """
    def bar
      User.count
    end
    """
    And I place the cursor between "Use" and "r"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/models/user.rb"

  Scenario: Going to a model from a namespaced ruby constant
    And file "app/models/admin/user.rb" exists
    And I clear the buffer and insert:
    """
    def bar
      Admin::User.count
    end
    """
    And I place the cursor between "Use" and "r"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/models/admin/user.rb"

  Scenario: Going to a lib from a namespaced ruby constant
    And file "lib/admin/logging.rb" exists
    And I clear the buffer and insert:
    """
    def bar
      Admin::Logging.object_id
    end
    """
    And I place the cursor between "Admin::Log" and "ging"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "lib/admin/logging.rb"

  Scenario: Going to ruby constant which is a controller
    And file "app/controllers/admin/users_controller.rb" exists
    And I clear the buffer and insert:
    """
    Admin::UsersController
    """
    And I place the cursor between "Admin::Use" and "rs"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/controllers/admin/users_controller.rb"

  Scenario: Going to ruby constant which is defined in app/jobs
    And file "app/jobs/admin/foo_bar_job.rb" exists
    And I clear the buffer and insert:
    """
    Admin::FooBarJob
    """
    And I place the cursor between "Foo" and "Bar"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/jobs/admin/foo_bar_job.rb"

  Scenario: Not going to non-existant model
    And I clear the buffer and insert:
    """
    def bar
      Bar.count
    end
    """
    And I place the cursor between "Ba" and "r"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/controllers/foos_controller.rb"

  Scenario: Going to a Concern
    And file "app/controllers/concerns/bar.rb" exists
    And I clear the buffer and insert:
    """
    include Bar
    """
    And I place the cursor between "Ba" and "r"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/controllers/concerns/bar.rb"
