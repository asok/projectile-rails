Feature: Filling new buffer with class definition
  In order to work faster
  As a user
  I want to be able to create a new file and have the buffer filled a class skeleton

Background:
  Given I open the app file "app/models/user.rb"
  And I turn on projectile-mode

Scenario: Opening new model
  When I open the app file "app/models/foo.rb"
  Then I should see:
  """
  class Foo < ActiveRecord::Base
    
  end
  """

Scenario: Opening new controller
  When I open the app file "app/controllers/foos_controller.rb"
  Then I should see:
  """
  class FoosController < ApplicationController
    
  end
  """

Scenario: Opening a new namespaced controller
  When I open the app file "app/controllers/admin/foos_controller.rb"
  Then I should see:
  """
  class Admin::FoosController < ApplicationController
    
  end
  """

Scenario: Opening a new lib
  When I open the app file "lib/fooing.rb"
  Then I should see:
  """
  class Fooing
    
  end
  """

Scenario: Opening a new namespaced lib
  When I open the app file "lib/admin/fooing.rb"
  Then I should see:
  """
  module Admin
    class Fooing
      
    end
  end
  """
