#This feature only passes with the sleeping step.
#And only when run with --win option
Feature: Font locking rails specific keywords
  In order to read the rails application source code
  As a user
  I want to see rails keywords highlighted
  
Scenario: Font locking models keywords
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode
  And I force font lock refresh
  Then I should see "validates_presence_of" font locked
  And I should see "alias_attribute" font locked

Scenario: Font locking controllers keywords
  Given I open the app file "app/controllers/users_controller.rb"
  And I turn on projectile-mode
  And I force font lock refresh
  Then I should see "before_filter" font locked
  And I should see "alias_attribute" font locked

Scenario: Font locking migrations keywords
  Given I open the app file "db/migrate/20131118160600_create_users.rb"
  And I turn on projectile-mode
  And I force font lock refresh
  Then I should see "create_table" font locked
  And I should see "alias_attribute" font locked
