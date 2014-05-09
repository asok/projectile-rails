Feature: Running rails server command

Scenario: Running rails server
  Given file "app/controllers/users_controller.rb" exists
  And file "app/views/users/_user.html.erb" exists
  When I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  And I simulate running server with output:
  """
  Processing by UsersController#show as HTML
    Rendered users/_user.html.erb (1.6ms)
  """
  When I place the cursor between "Users" and "Controller"
  And I press "RET"
  Then I am in file "app/controllers/users_controller.rb"
  When I switch to buffer "*projectile-rails-server*"
  And I place the cursor between "users" and "/_user"
  And I press "RET"
  Then I am in file "app/views/users/_user.html.erb"
