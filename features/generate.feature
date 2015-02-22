Feature: Using rails generate command
  In order to generate projects's files
  As a user
  I want to be able to run rails generate command from Emacs

Scenario: Runnning generate rspec:install
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run command "projectile-rails-generate" inputting "rspec:install"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "bundle exec rails generate rspec:install"

Scenario: Running generate rspec:install when spring is running
  And spring is running
  And I turn on projectile-mode
  When I run command "projectile-rails-generate" inputting "rspec:install"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "spring rails generate rspec:install"

Scenario: Running generate rspec:install when zeus is running
  And zeus is running with the default location for the socket file
  And I turn on projectile-mode
  When I run command "projectile-rails-generate" inputting "rspec:install"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "zeus generate rspec:install"

Scenario: Running generate rspec:install when zeus is running
  And zeus is running with the non-default location for the socket file
  And I turn on projectile-mode
  When I run command "projectile-rails-generate" inputting "rspec:install"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "zeus generate rspec:install"

Scenario: Using buttons
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  And file "spec/spec_helper.rb" exists
  And I simulate generating "rspec:install" with output:
  """
  be rails g rspec:install
      create  .rspec
       exist  spec
    conflict  spec/spec_helper.rb
        skip  spec/spec_helper.rb
  """
  When I place the cursor between "conflict  spe" and "c"
  And I press "RET"
  Then I am in file "spec/spec_helper.rb"

Scenario: Running generate with input 'as' should generate assets
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run projectile-rails-generate inputting "as" and the name "post"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "bundle exec rails generate assets post"

Scenario: Running generate with input 'co' should generate a controller
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run projectile-rails-generate inputting "co" and the name "posts"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "bundle exec rails generate controller post"

Scenario: Running generate with input 'ge' should generate a generator
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run projectile-rails-generate inputting "ge" and the name "post"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "bundle exec rails generate generator post"

Scenario: Running generate with input 'he' should generate a helper
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run projectile-rails-generate inputting "he" and the name "post"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "bundle exec rails generate helper post"

Scenario: Running generate with input 'int' should generate an integration_test
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run projectile-rails-generate inputting "int" and the name "post"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "bundle exec rails generate integration_test post"

Scenario: Running generate with input 'jo' should generate a job
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run projectile-rails-generate inputting "jo" and the name "post"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "bundle exec rails generate job post"

Scenario: Running generate with input 'ma' should generate a mailer
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run projectile-rails-generate inputting "ma" and the name "post"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "bundle exec rails generate mailer post"

Scenario: Running generate with input 'mig' shuold generate a migration
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run projectile-rails-generate inputting "mig" and the name "post"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "bundle exec rails generate migration post"

Scenario: Running generate with input 'mo' should generate a model
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run projectile-rails-generate inputting "mo" and the name "post"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "bundle exec rails generate model post"

Scenario: Runniing generate with input 're' should generate a resource
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run projectile-rails-generate inputting "re" and the name "post"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "bundle exec rails generate resource post"

Scenario: Running generate with input 'sc' should generate a scaffold
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run projectile-rails-generate inputting "sc" and the name "post"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "bundle exec rails generate scaffold post"

Scenario: Running generate with input 'ta' should generate a task
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  When I run projectile-rails-generate inputting "ta" and the name "post"
  And I switch to buffer "*projectile-rails-generate*"
  Then I should see "bundle exec rails generate task post"
