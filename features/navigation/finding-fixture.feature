@finding-fixtures
Feature: Finding a fixture file

Scenario: Finding the users fixtures when using test
  Given I open the app file "app/models/user.rb"
  And directory "test/fixtures/" exists
  And file "test/fixtures/users.yml" exists
  And I turn on projectile-mode
  When I run command "projectile-rails-find-fixture" selecting "users"
  Then I am in file "test/fixtures/users.yml"

Scenario: Finding the users fixtures when using rspec
  Given I open the app file "app/models/user.rb"
  And directory "spec/fixtures/" exists
  And file "spec/fixtures/users.yml" exists
  And I turn on projectile-mode
  When I run command "projectile-rails-find-fixture" selecting "users"
  Then I am in file "spec/fixtures/users.yml"

Scenario: Finding the users factories when using test
  Given I open the app file "app/models/user.rb"
  And directory "test/factories/" exists
  And file "test/factories/users.rb" exists
  And I turn on projectile-mode
  When I run command "projectile-rails-find-fixture" selecting "users"
  Then I am in file "test/factories/users.rb"

Scenario: Finding the users factories when using rspec
  Given I open the app file "app/models/user.rb"
  And directory "spec/factories/" exists
  And file "spec/factories/users.rb" exists
  And I turn on projectile-mode
  When I run command "projectile-rails-find-fixture" selecting "users"
  Then I am in file "spec/factories/users.rb"

Scenario: Finding the users fabricator when using test
  Given I open the app file "app/models/user.rb"
  And directory "test/fabricators/" exists
  And file "test/fabricators/user_fabricator.rb" exists
  And I turn on projectile-mode
  When I run command "projectile-rails-find-fixture" selecting "user"
  Then I am in file "test/fabricators/user_fabricator.rb"

Scenario: Finding the users fabricator when using rspec
  Given I open the app file "app/models/user.rb"
  And directory "spec/fabricators/" exists
  And file "spec/fabricators/user_fabricator.rb" exists
  And I turn on projectile-mode
  When I run command "projectile-rails-find-fixture" selecting "user"
  Then I am in file "spec/fabricators/user_fabricator.rb"

Scenario: Finding the current fixture when using test
  Given I open the app file "app/models/user.rb"
  And directory "test/fixtures/" exists
  And file "test/fixtures/users.yml" exists
  And I turn on projectile-mode
  When I run "projectile-rails-find-current-fixture"
  Then I am in file "test/fixtures/users.yml"

Scenario: Finding the current fixture when using rspec
  Given I open the app file "app/models/user.rb"
  And directory "spec/fixtures/" exists
  And file "spec/fixtures/users.yml" exists
  And I turn on projectile-mode
  When I run "projectile-rails-find-current-fixture"
  Then I am in file "spec/fixtures/users.yml"

Scenario: Finding the current factory when using test
  Given I open the app file "app/models/user.rb"
  And directory "test/factories/" exists
  And file "test/factories/users.rb" exists
  And I turn on projectile-mode
  When I run "projectile-rails-find-current-fixture"
  Then I am in file "test/factories/users.rb"

Scenario: Finding the current factory when using rspec
  Given I open the app file "app/models/user.rb"
  And directory "spec/factories/" exists
  And file "spec/factories/users.rb" exists
  And I turn on projectile-mode
  When I run "projectile-rails-find-current-fixture"
  Then I am in file "spec/factories/users.rb"

Scenario: Finding the current fabricator when using test
  Given I open the app file "app/models/user.rb"
  And directory "test/fabricators/" exists
  And file "test/fabricators/user_fabricator.rb" exists
  And I turn on projectile-mode
  When I run "projectile-rails-find-current-fixture"
  Then I am in file "test/fabricators/user_fabricator.rb"

Scenario: Finding the current fabricator when using rspec
  Given I open the app file "app/models/user.rb"
  And directory "spec/fabricators/" exists
  And file "spec/fabricators/user_fabricator.rb" exists
  And I turn on projectile-mode
  When I run "projectile-rails-find-current-fixture"
  Then I am in file "spec/fabricators/user_fabricator.rb"
