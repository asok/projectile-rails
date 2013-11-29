Feature: Going from a ruby file from require line
  In order to do find fast required file or gem
  As a user
  I want to be able to run projectile-rails command and jump to the file or gem at point

Scenario: Going at: require_relative './admin/logging'
  And I open the app file "lib/logging.rb"
  And I clear the buffer and insert:
  """
  require_relative './admin/logging'
  """
  And I place the cursor between "log" and "ging"
  When I run "projectile-rails-goto-file-at-point"
  Then I am in file "lib/admin/logging.rb"

Scenario: Going at: require_relative 'admin/logging'
  And I open the app file "lib/logging.rb"
  And I clear the buffer and insert:
  """
  require_relative 'admin/logging'
  """
  And I place the cursor between "log" and "ging"
  When I run "projectile-rails-goto-file-at-point"
  Then I am in file "lib/admin/logging.rb"
