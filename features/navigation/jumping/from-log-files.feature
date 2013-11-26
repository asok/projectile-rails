Feature: Jumping from a line in a log file
  In order to do find fast files at point in the log file
  As a user
  I want to be able to run projectile-rails command and jump to the correct file
  
  Scenario: Jumping from: Processing by Admin/UsersController#new as HTML
    Given I open the app file "app/models/user.rb"
    And I turn on projectile-mode
    And I clear the buffer and insert:
    """
    Processing by Admin::UsersController#new as HTML
    """
    And I place the cursor between "Admin::Use" and "rs"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "app/controllers/admin/users_controller.rb"

  Scenario: Jumping from: Rendered users/index.html.erb (43.5ms)
    Given I open the app file "app/models/user.rb"
    And I turn on projectile-mode
    And I clear the buffer and insert:
    """
        Rendered users/index.html.erb (43.5ms)
    """
    And I place the cursor between "users/in" and "dex"
    When I run "projectile-rails-ff-at-point"
    Then I am in file "app/views/users/index.html.erb"
