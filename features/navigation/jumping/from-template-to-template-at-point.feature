Feature: Jumping from a template to a template at point
  In order to do find fast templates and partials at point
  As a user
  I want to be able to run projectile-rails command and jump to the template or partial at point
  
Background:
  Given I turn on projectile-mode
  
  Scenario: Jumping from: render 'index'
    And I open the app file "app/views/users/new.html.erb"
    And I clear the buffer and insert:
    """
    render 'index'
    """
    And I place the cursor between "ind" and "ex"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/views/users/index.html.erb"

  Scenario: Jumping from: render 'users/index'
    And I open the app file "app/views/users/new.html.erb"
    And I clear the buffer and insert:
    """
    render 'users/index'
    """
    And I place the cursor between "ind" and "ex"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/views/users/index.html.erb"

  Scenario: Jumping from: render 'admin/users/index'
    And I open the app file "app/views/users/new.html.erb"
    And I clear the buffer and insert:
    """
    render 'admin/users/index'
    """
    And I place the cursor between "ind" and "ex"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/views/admin/users/index.html.haml"

  Scenario: Jumping from: render 'users/user'
    And I open the app file "app/views/users/new.html.erb"
    And I clear the buffer and insert:
    """
    render 'users/user'
    """
    And I place the cursor between "use" and "r"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/views/users/_user.html.erb"

  Scenario: Jumping from: render 'user'
    And I open the app file "app/views/users/new.html.erb"
    And I clear the buffer and insert:
    """
    render 'user'
    """
    And I place the cursor between "use" and "r"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/views/users/_user.html.erb"
