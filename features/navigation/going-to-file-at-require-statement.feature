Feature: Going from a ruby file from require line
  In order to do find fast required file or gem
  As a user
  I want to be able to run projectile-rails command and jump to the file or gem at point

Scenario: Going at: require_relative './admin/logging'
  Given file "lib/admin/logging.rb" exists
  And I open the app file "lib/logging.rb"
  And I turn on projectile-mode
  And I clear the buffer and insert:
  """
  require_relative './admin/logging'
  """
  And I place the cursor between "log" and "ging"
  When I run "projectile-rails-goto-file-at-point"
  Then I am in file "lib/admin/logging.rb"

Scenario: Going at: require_relative 'admin/logging'
  Given file "lib/admin/logging.rb" exists
  And I open the app file "lib/logging.rb"
  And I turn on projectile-mode
  And I clear the buffer and insert:
  """
  require_relative 'admin/logging'
  """
  And I place the cursor between "log" and "ging"
  When I run "projectile-rails-goto-file-at-point"
  Then I am in file "lib/admin/logging.rb"

Scenario: Going to gem at line: require 'foo'
  Given there is foo gem in directory "vendor/foo/"
  And I open the app file "Gemfile"
  And I clear the buffer and insert:
  """
  gem 'foo', '0.0.0', path: './vendor/foo'
  """
  And I save the buffer
  And I open the app file "app/models/user.rb"
  And I clear the buffer and insert:
  """
  require 'foo'
  """
  When I place the cursor between "'fo" and "o'"
  And I turn on projectile-mode
  And I run "projectile-rails-goto-file-at-point"
  Then I am in a dired buffer "vendor/foo/"
