Feature: Finding resources
  In order to do easly find rails resources
  As a user
  I want to be able to run projectile-rails command and choose a file to open

  Background:
    Given I open the app file "app/models/user.rb"
    And I turn on projectile-mode

  Scenario: Switching to a model
    And file "app/models/group.rb" exists
    When I run command "projectile-rails-find-model" selecting "group.rb"
    Then I am in file "app/models/group.rb"

  Scenario: Switching to a controller
    And file "app/controllers/groups_controller.rb" exists
    When I run command "projectile-rails-find-controller" selecting "groups_controller.rb"
    Then I am in file "app/controllers/groups_controller.rb"

  Scenario: Finding views
    And file "app/views/users/index.html.erb" exists
    When I run command "projectile-rails-find-view" selecting "users/index.html.erb"
    Then I am in file "app/views/users/index.html.erb"

  Scenario: Finding helpers
    And file "app/helpers/users_helper.rb" exists
    When I run command "projectile-rails-find-helper" selecting "users_helper.rb"
    Then I am in file "app/helpers/users_helper.rb"

  Scenario: Finding libs
    And file "lib/logging.rb" exists
    When I run command "projectile-rails-find-lib" selecting "logging.rb"
    Then I am in file "lib/logging.rb"

  Scenario: Finding specs
    And file "spec/models/user_spec.rb" exists
    When I run command "projectile-rails-find-spec" selecting "models/user_spec.rb"
    Then I am in file "spec/models/user_spec.rb"

