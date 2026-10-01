Feature: Extracting region

Scenario: Extracting a region to a partial
  Given file "app/views/admin/users/index.html.erb" exists
  And I open the app file "app/views/admin/users/index.html.erb"
  And I clear the buffer and insert:
  """
  <div>
    <span>To extract</span>
  </div>
  """
  When I turn on projectile-rails-mode
  And I select "<span>To extract</span>"
  And I run command "projectile-rails-extract-region" inputting "_user.html.erb"
  Then I am in file "app/views/admin/users/_user.html.erb"
  And I should see "<span>To extract</span>"
  When I run "kill-buffer"
  And I open the app file "app/views/admin/users/index.html.erb"
  Then I should not see "<span>To extract</span>"
  And I should see:
  """
  <div>
    <%= render 'admin/users/user' %>
  </div>
  """

Scenario: Extracting a region to a partial with a single extension
  Given file "app/views/admin/users/index.html.erb" exists
  And I open the app file "app/views/admin/users/index.html.erb"
  And I clear the buffer and insert:
  """
  <div>
    <span>To extract</span>
  </div>
  """
  When I turn on projectile-rails-mode
  And I select "<span>To extract</span>"
  And I run command "projectile-rails-extract-region" inputting "_user.erb"
  Then I am in file "app/views/admin/users/_user.erb"
  And I should see "<span>To extract</span>"
  When I run "kill-buffer"
  And I open the app file "app/views/admin/users/index.html.erb"
  Then I should not see "<span>To extract</span>"
  And I should see:
  """
  <div>
    <%= render 'admin/users/user' %>
  </div>
  """

Scenario: Extracting a region to a partial under a directory with a dot
  Given directory "app/views/admin/v1.2/" exists
  And file "app/views/admin/v1.2/index.html.erb" exists
  And I open the app file "app/views/admin/v1.2/index.html.erb"
  And I clear the buffer and insert:
  """
  <div>
    <span>To extract</span>
  </div>
  """
  When I turn on projectile-rails-mode
  And I select "<span>To extract</span>"
  And I run command "projectile-rails-extract-region" inputting "_user.html.erb"
  Then I am in file "app/views/admin/v1.2/_user.html.erb"
  And I should see "<span>To extract</span>"
  When I run "kill-buffer"
  And I open the app file "app/views/admin/v1.2/index.html.erb"
  Then I should not see "<span>To extract</span>"
  And I should see:
  """
  <div>
    <%= render 'admin/v1.2/user' %>
  </div>
  """
