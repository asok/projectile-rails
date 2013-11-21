Feature: Finding controllers
  In order to do easly find rails resources
  As a user
  I want to be able to find controllers

  Background:
    Given I open the app file "app/models/user.rb"
    And I turn on projectile-mode

  Scenario: Finding views
    When I run command "projectile-rails-views" selecting "users/index.html.erb"
    Then I am in file "app/views/users/index.html.erb"

  Scenario: Seeing correct views
    And I am using a test completion system
    When I run "projectile-rails-views"
    Then the completions should be:
    """
    admin/users/index.html.haml
    groups/new.js.erb
    layouts/application.html.erb
    users/index.html.erb
    """
