Feature: Going to a javascript file from assets pipeline manifest

Scenario: Going to file from: //= require user
    Given file "app/assets/javascripts/user.js" exists
    And I open the app file "app/assets/javascripts/application.js"
    And I turn on projectile-mode
    And I clear the buffer and insert:
    """
     //= require user
    """
    And I place the cursor between "use" and "r"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/assets/javascripts/user.js"

Scenario: Going to file from: //= require ./user
    Given file "app/assets/javascripts/user.js" exists
    And I open the app file "app/assets/javascripts/application.js"
    And I turn on projectile-mode
    And I clear the buffer and insert:
    """
     //= require ./user
    """
    And I place the cursor between "use" and "r"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/assets/javascripts/user.js"

Scenario: Going to file in app/assets/foo directory from: //= require foo/bar
    Given file "app/assets/javascripts/foo/" exists
    And file "app/assets/javascripts/foo/bar.js" exists
    And I open the app file "app/assets/javascripts/application.js"
    And I turn on projectile-mode
    And I clear the buffer and insert:
    """
     //= require foo/bar
    """
    And I place the cursor between "fo" and "o"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "app/assets/javascripts/foo/bar.js"

Scenario: Going to file in lib/assets/bar directory from: //= require bar/baz
    Given file "lib/assets/javascripts/bar/" exists
    And file "lib/assets/javascripts/bar/baz.js" exists
    And I open the app file "app/assets/javascripts/application.js"
    And I turn on projectile-mode
    And I clear the buffer and insert:
    """
     //= require bar/baz
    """
    And I place the cursor between "ba" and "z"
    When I run "projectile-rails-goto-file-at-point"
    Then I am in file "lib/assets/javascripts/bar/baz.js"
