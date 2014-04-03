Feature: Extracting to partial
  
Scenario: Extracting a region to a partial
  Given file "app/views/users/index.html.erb" exists
  And I open the app file "app/views/users/index.html.erb"
  And I insert:
  """
  <div>
    <span>To extract</span>
  </div>
  """
  When I turn on projectile-mode
  And I select "<span>To extract</span>"
  And I run command "projectile-rails-extract-region" inputting "user.html.erb"
  Then I am in file "app/views/users/_user.html.erb"
  And I should see "<span>To extract</span>"
  When I run "kill-buffer"
  And I switch to buffer "index.html.erb" 
  Then I should not see "<span>To extract</span>"

