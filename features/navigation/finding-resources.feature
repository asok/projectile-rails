Feature: Finding resources
  In order to do easly find rails resources
  As a user
  I want to be able to run projectile-rails command and choose a file to open

  Background:
    Given I open the app file "app/models/user.rb"
    And I turn on projectile-mode

  Scenario: Finding model
    And file "app/models/group.rb" exists
    When I run command "projectile-rails-find-model" selecting "group"
    Then I am in file "app/models/group.rb"

  Scenario: Finding controller
    And file "app/controllers/groups_controller.rb" exists
    When I run command "projectile-rails-find-controller" selecting "groups"
    Then I am in file "app/controllers/groups_controller.rb"

  Scenario: Finding views
    And file "app/views/users/index.html.erb" exists
    When I run command "projectile-rails-find-view" selecting "users/index.html"
    Then I am in file "app/views/users/index.html.erb"

  Scenario: Finding helpers
    And file "app/helpers/users_helper.rb" exists
    When I run command "projectile-rails-find-helper" selecting "users"
    Then I am in file "app/helpers/users_helper.rb"

  Scenario: Finding libs
    And file "lib/logging.rb" exists
    When I run command "projectile-rails-find-lib" selecting "logging"
    Then I am in file "lib/logging.rb"

  Scenario: Finding specs
    And file "spec/models/user_spec.rb" exists
    When I run command "projectile-rails-find-spec" selecting "models/user"
    Then I am in file "spec/models/user_spec.rb"
    
  Scenario: Finding migration
    And file "db/migrate/12345678901234_create_users.rb" exists
    When I run command "projectile-rails-find-migration" selecting "12345678901234_create_users"
    Then I am in file "db/migrate/12345678901234_create_users.rb"

