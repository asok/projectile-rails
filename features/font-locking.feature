Feature: Font locking rails specific keywords
  In order to read the rails application source code
  As a user
  I want to see rails keywords highlighted

Scenario: Font locking models keywords
  Given file "app/models/user.rb" exists
  And I open the app file "app/models/user.rb"
  And I insert:
  """
  validates_presence_of
  alias_attribute
  """
  When I turn on projectile-mode
  And I force font lock refresh
  Then I should see "validates_presence_of" font locked
  And I should see "alias_attribute" font locked

Scenario: Font locking controllers keywords
  Given file "app/controllers/users_controller.rb" exists
  And I open the app file "app/controllers/users_controller.rb"
  And I insert:
  """
  before_filter
  alias_attribute
  """
  When I turn on projectile-mode
  And I force font lock refresh
  Then I should see "before_filter" font locked
  And I should see "alias_attribute" font locked

Scenario: Font locking migrations keywords
  Given file "db/migrate/20131118160600_create_users.rb" exists
  And I open the app file "db/migrate/20131118160600_create_users.rb"
  And I insert:
  """
  create_table
  alias_attribute
  """
  When I turn on projectile-mode
  And I force font lock refresh
  Then I should see "create_table" font locked
  And I should see "alias_attribute" font locked

Scenario: Disabling the feature
  Given file "db/migrate/20131118160600_create_users.rb" exists
  And I open the app file "db/migrate/20131118160600_create_users.rb"
  And I insert:
  """
  alias_attribute
  """
  When I turn off adding keywords
  And I turn on projectile-mode
  And I force font lock refresh
  And I should not see "alias_attribute" font locked
