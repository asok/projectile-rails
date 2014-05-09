Feature: Going to a stylesheet file from assets pipeline manifest

Scenario: Going to file from: *= require user
    Given file "app/assets/stylesheets/user.css" exists
    And I open the app file "app/assets/stylesheets/application.css"
    And I turn on projectile-mode
    And I insert:
    """
     *= require user
    """
    And I place the cursor between "use" and "r"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/assets/stylesheets/user.css"

Scenario: Going to file from: *= require ./user
    Given file "app/assets/stylesheets/user.css" exists
    And I open the app file "app/assets/stylesheets/application.css"
    And I turn on projectile-mode
    And I clear the buffer and insert:
    """
     *= require ./user
    """
    And I place the cursor between "use" and "r"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/assets/stylesheets/user.css"

Scenario: Going to file in foo directory from: *= require foo/bar
    Given file "app/assets/stylesheets/foo/" exists
    And file "app/assets/stylesheets/foo/bar.css" exists
    And I open the app file "app/assets/stylesheets/application.css"
    And I turn on projectile-mode
    And I clear the buffer and insert:
    """
     *= require foo/bar
    """
    And I place the cursor between "fo" and "o"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/assets/stylesheets/foo/bar.css"

Scenario: Going to file in lib/assets/bar directory from: *= require bar/baz
    Given file "lib/assets/stylesheets/bar/" exists
    And file "lib/assets/stylesheets/bar/baz.css" exists
    And I open the app file "app/assets/stylesheets/application.css"
    And I turn on projectile-mode
    And I clear the buffer and insert:
    """
     *= require bar/baz
    """
    And I place the cursor between "ba" and "z"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "lib/assets/stylesheets/bar/baz.css"
