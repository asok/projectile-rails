Feature: Keybindings

  Scenario: Having the find keybindings setup after enabling the mode
    Given I open the app file "app/models/foo.rb"
    When I turn on projectile-mode
    Then key "C-c r m" should be mapped to "projectile-rails-find-model"
    And  key "C-c r M" should be mapped to "projectile-rails-find-current-model"
    And  key "C-c r c" should be mapped to "projectile-rails-find-controller"
    And  key "C-c r C" should be mapped to "projectile-rails-find-current-controller"
    And  key "C-c r v" should be mapped to "projectile-rails-find-view"
    And  key "C-c r V" should be mapped to "projectile-rails-find-current-view"
    And  key "C-c r h" should be mapped to "projectile-rails-find-helper"
    And  key "C-c r H" should be mapped to "projectile-rails-find-current-helper"
    And  key "C-c r l" should be mapped to "projectile-rails-find-lib"
    And  key "C-c r f" should be mapped to "projectile-rails-find-feature"
    And  key "C-c r u" should be mapped to "projectile-rails-find-fixture"
    And  key "C-c r U" should be mapped to "projectile-rails-find-current-fixture"
    And  key "C-c r p" should be mapped to "projectile-rails-find-spec"
    And  key "C-c r P" should be mapped to "projectile-rails-find-current-spec"
    And  key "C-c r t" should be mapped to "projectile-rails-find-test"
    And  key "C-c r T" should be mapped to "projectile-rails-find-current-test"
    And  key "C-c r n" should be mapped to "projectile-rails-find-migration"
    And  key "C-c r N" should be mapped to "projectile-rails-find-current-migration"
    And  key "C-c r i" should be mapped to "projectile-rails-find-initializer"
    And  key "C-c r j" should be mapped to "projectile-rails-find-javascript"
    And  key "C-c r s" should be mapped to "projectile-rails-find-stylesheet"
    And  key "C-c r o" should be mapped to "projectile-rails-find-log"
    And  key "C-c r e" should be mapped to "projectile-rails-find-environment"
    And  key "C-c r @" should be mapped to "projectile-rails-find-mailer"
    And  key "C-c r y" should be mapped to "projectile-rails-find-layout"
    
  Scenario: Having the commands keybindings setup after enabling the mode
    Given I open the app file "app/models/foo.rb"
    When I turn on projectile-mode
    Then key "C-c r ! c" should be mapped to "projectile-rails-console"
    And  key "C-c r r" should be mapped to "projectile-rails-console"
    And  key "C-c r ! s" should be mapped to "projectile-rails-server"
    And  key "C-c r R" should be mapped to "projectile-rails-server"
    And  key "C-c r ! r" should be mapped to "projectile-rails-rake"
    And  key "C-c r ! g" should be mapped to "projectile-rails-generate"
    And  key "C-c r x" should be mapped to "projectile-rails-extract-region"

  Scenario: Having the goto keybindings setup after enabling the mode
    Given I open the app file "app/models/foo.rb"
    When I turn on projectile-mode
    Then key "C-c r RET" should be mapped to "projectile-rails-goto-file-at-point"
    And  key "C-c r g f" should be mapped to "projectile-rails-goto-file-at-point"
    And  key "C-c r g g" should be mapped to "projectile-rails-goto-gemfile"
    And  key "C-c r g r" should be mapped to "projectile-rails-goto-routes"
    And  key "C-c r g s" should be mapped to "projectile-rails-goto-seeds"
    And  key "C-c r g h" should be mapped to "projectile-rails-goto-spec-helper"
