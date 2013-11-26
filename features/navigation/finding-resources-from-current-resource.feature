Feature: Finding resources based on current resource
  In order to do easly find rails resources
  As a user
  I want to be able to find resources connected with the file I'm currently visiting
  
  Scenario: Going from models
    Given I open the app file "app/models/user.rb"
    And I turn on projectile-mode
    When I run command "projectile-rails-find-current-resource" selecting "app/controllers/users_controller.rb"
    Then I am in file "app/controllers/users_controller.rb"
      
  Scenario: Going from controllers
    Given I open the app file "app/controllers/users_controller.rb"
    And I turn on projectile-mode
    When I run command "projectile-rails-find-current-resource" selecting "app/models/user.rb"
    Then I am in file "app/models/user.rb"

  Scenario: Going from views
    Given I open the app file "app/views/users/index.html.erb"
    And I turn on projectile-mode
    When I run command "projectile-rails-find-current-resource" selecting "app/models/user.rb"
    Then I am in file "app/models/user.rb"

  Scenario: Going from helpers
    Given I open the app file "app/helpers/users_helper.rb"
    And I turn on projectile-mode
    When I run command "projectile-rails-find-current-resource" selecting "app/models/user.rb"
    Then I am in file "app/models/user.rb"
      
  Scenario: Going from libs
    Given I open the app file "lib/logging.rb"
    And I turn on projectile-mode
    When I run command "projectile-rails-find-current-resource" selecting "spec/lib/logging_spec.rb"
    Then I am in file "spec/lib/logging_spec.rb"
      
  Scenario: Going from specs
    Given I open the app file "spec/models/user_spec.rb"
    And I turn on projectile-mode
    When I run command "projectile-rails-find-current-resource" selecting "app/models/user.rb"
    Then I am in file "app/models/user.rb"
