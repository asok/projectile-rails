Feature: Finding current resource
  In order to go to current resources
  As a user
  I want to be able to find resources connected with the file I'm currently visiting

  Scenario: Finding current model
    Given file "app/models/user.rb" exists
    And I open the app file "app/controllers/users_controller.rb"
    And I turn on projectile-mode
    When I run "projectile-rails-find-current-model"
    Then I am in file "app/models/user.rb"

  Scenario: Finding current controller
    Given file "app/controllers/users_controller.rb" exists
    And file "app/controllers/admin/users_controller.rb" exists
    And I open the app file "app/models/user.rb"
    And I turn on projectile-mode
    When I run command "projectile-rails-find-current-controller" selecting "users"
    Then I am in file "app/controllers/users_controller.rb"

  Scenario: Finding current view
    Given file "app/views/users/index.html.erb" exists
    And file "app/views/users/new.html.erb" exists
    And I open the app file "app/models/user.rb"
    And I turn on projectile-mode
    When I run command "projectile-rails-find-current-view" selecting "new.html.erb"
    Then I am in file "app/views/users/new.html.erb"

  Scenario: Finding current helper
    Given file "app/helpers/users_helper.rb" exists
    And I open the app file "app/models/user.rb"
    And I turn on projectile-mode
    When I run "projectile-rails-find-current-helper"
    Then I am in file "app/helpers/users_helper.rb"

  Scenario: Finding current spec
    Given file "spec/controllers/users_controller_spec.rb" exists
    And file "spec/controllers/admin/users_controller_spec.rb" exists
    And I open the app file "app/controllers/users_controller.rb"
    And I turn on projectile-mode
    When I run "projectile-rails-find-current-spec"
    Then I am in file "spec/controllers/users_controller_spec.rb"

  Scenario: Finding current migration
    Given file "app/models/user.rb" exists
    And file "db/migrate/12345678901234_create_users.rb" exists
    And I open the app file "app/models/user.rb"
    And I turn on projectile-mode
    When I run "projectile-rails-find-current-migration"
    Then I am in file "db/migrate/12345678901234_create_users.rb"

  Scenario: Finding current model from migration
    Given file "db/migrate/12345678901234_create_users.rb" exists
    And file "app/models/user.rb" exists
    And I open the app file "db/migrate/12345678901234_create_users.rb"
    And I turn on projectile-mode
    When I run "projectile-rails-find-current-model"
    Then I am in file "app/models/user.rb"

  Scenario: Finding current model when there is no model for current resource
    Given file "app/models/user.rb" exists
    And I open the app file "app/controllers/foos_controller.rb"
    And I turn on projectile-mode
    When I run command "projectile-rails-find-current-model" selecting "user"
    Then I am in file "app/models/user.rb"
