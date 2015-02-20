Feature: Using rails destroy command
  In order to destroy projects's files
  As a user
  I want to be able to run rails destroy command from Emacs

Scenario: Runnning destroy model airplane
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run command "projectile-rails-destroy" inputting "model airplane"
  And I switch to buffer "*projectile-rails-compilation*"
  Then I should see "bundle exec rails destroy model airplane"

Scenario: Running destroy model airplane when spring is running
  And spring is running
  And I turn on projectile-mode
  When I run command "projectile-rails-destroy" inputting "model airplane"
  And I switch to buffer "*projectile-rails-compilation*"
  Then I should see "spring rails destroy model airplane"

Scenario: Running destroy model airplane when zeus is running
  And zeus is running with the default location for the socket file
  And I turn on projectile-mode
  When I run command "projectile-rails-destroy" inputting "model airplane"
  And I switch to buffer "*projectile-rails-compilation*"
  Then I should see "zeus destroy model airplane"

Scenario: Running destroy model airplane when zeus is running
  And zeus is running with the non-default location for the socket file
  And I turn on projectile-mode
  When I run command "projectile-rails-destroy" inputting "model airplane"
  And I switch to buffer "*projectile-rails-compilation*"
  Then I should see "zeus destroy model airplane"

Scenario: Running destroy model airplane with completion
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run projectile-rails-destroy inputting "mo" and selecting "airplane"
  And I switch to buffer "*projectile-rails-compilation*"
  Then I should see "bundle exec rails destroy model airplane"

