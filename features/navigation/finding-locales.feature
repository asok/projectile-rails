Feature: Finding a locale file

Scenario: Finding locale file
  Given I open the app file "app/models/user.rb"
  And file "config/locales/en.yml" exists
  And file "config/locales/pl.rb" exists
  And file "config/locales/es.yml" exists
  And I turn on projectile-mode
  When I run command "projectile-rails-find-locale" selecting "pl"
  Then I am in file "config/locales/pl.rb"
