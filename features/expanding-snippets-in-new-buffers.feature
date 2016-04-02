Feature: Filling new buffer with class definition
  In order to work faster
  As a user
  I want to be able to create a new file and have the buffer filled a class skeleton

Background:
  Given I turn on snippet expansion

Scenario: Opening new model
  When I open the app file "app/models/foo.rb"
  And I turn on projectile-mode
  Then I should see:
  """
  class Foo < ActiveRecord::Base
    
  end
  """

Scenario: Opening new controller
  When I open the app file "app/controllers/foos_controller.rb"
  And I turn on projectile-mode
  Then I should see:
  """
  class FoosController < ApplicationController
    
  end
  """

Scenario: Opening a new namespaced controller
  Given directory "app/controllers/admin/" exists
  When I open the app file "app/controllers/admin/foos_controller.rb"
  And I turn on projectile-mode
  Then I should see:
  """
  class Admin::FoosController < ApplicationController
    
  end
  """

Scenario: Opening a new lib
  When I open the app file "lib/fooing.rb"
  And I turn on projectile-mode
  Then I should see:
  """
  module Fooing
    
  end
  """

Scenario: Opening a new file under the app directory
  When I open the app file "app/jobs/admin/foo_job.rb"
  And I turn on projectile-mode
  Then I should see:
  """
  module Admin
    class FooJob
      
    end
  end
  """

Scenario: Opening a new namespaced lib
  Given directory "lib/admin/" exists
  When I open the app file "lib/admin/fooing.rb"
  And I turn on projectile-mode
  Then I should see:
  """
  module Admin
    module Fooing
      
    end
  end
  """

Scenario: Opening a new spec
  When I open the app file "spec/models/bar_spec.rb"
  And I turn on projectile-mode
  Then I should see:
  """
  require "rails_helper"
  
  RSpec.describe Bar do
    
  end
  """

Scenario: Opening a new concern
  When I open the app file "app/models/concerns/foo.rb"
  And I turn on projectile-mode
  Then I should see:
  """
  module Foo
    extend ActiveSupport::Concern
    
  end
  """

Scenario: Opening a new spec and the buffer is not empty
  When I open the app file "spec/models/bar_spec.rb"
  And I insert:
  """
  Emacs!
  """
  And I turn on projectile-mode
  Then I should not see:
  """
  require "spec_helper"
  """

Scenario: Disabling the feature
  Given I turn off snippet expansion
  When I open the app file "spec/models/bar_spec.rb"
  And I turn on projectile-mode
  Then I should not see:
  """
  require "spec_helper"
  """
