Feature: Going from a controller to a template at point
  In order to do find fast templates and partials at point
  As a user
  I want to be able to run projectile-rails command and jump to the template or partial at point

  Scenario: Going from: format.html { render 'users/index' }
    Given file "app/views/admin/users/new.html.haml" exists
    And I open the app file "app/controllers/admin/users_controller.rb"
    And I turn on projectile-mode
    And I clear the buffer and insert:
    """
    render :new
    """
    And I place the cursor between ":ne" and "w"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in a dired buffer "app/views/admin/users/"

  Scenario: Going from: render :new, formats: [:js]
    Given file "app/views/admin/users/new.js.slim" exists
    And I open the app file "app/controllers/admin/users_controller.rb"
    And I turn on projectile-mode
    And I clear the buffer and insert:
    """
    render :new, formats: [:js]
    """
    And I place the cursor between "ne" and "w"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/views/admin/users/new.js.slim"
