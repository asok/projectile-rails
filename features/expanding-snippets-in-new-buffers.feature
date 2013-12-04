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
  When I open the app file "spec/models/user_spec.rb"
  And I turn on projectile-mode
  Then I should see:
  """
  require "spec_helper"

  describe User do
    
  end
  """
