Feature: Using rake
  In order to use rake
  As a user
  I want to be able to run a rake tasks from Emacs

Background:
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode

Scenario: Running about task
  When I run command "projectile-rails-rake" selecting "about"
  And I switch to buffer "*projectile-rails-compilation*"
  Then I should see "bundle exec rake about"
