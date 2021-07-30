Feature: Filling new buffer with class definition
  In order to work faster
  As a user
  I want to be able to create a new file and have the buffer filled a class skeleton

Background:
  Given I turn on snippet expansion
  And I turn on auto-insert-mode

Scenario: Opening new model
  When I turn on projectile-rails-mode
  And I open the app file "app/models/new_model.rb"
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  class NewModel < ActiveRecord::Base

  end
  """

Scenario: Opening new model when ApplicationRecord exists
  Given file "app/models/application_record.rb" exists
  When I turn on projectile-rails-mode
  And I open the app file "app/models/bar.rb"
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  class Bar < ApplicationRecord

  end
  """

Scenario: Opening new controller
  When I turn on projectile-rails-mode
  And I open the app file "app/controllers/foos_controller.rb"
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  class FoosController < ApplicationController

  end
  """

Scenario: Opening a new namespaced controller
  Given directory "app/controllers/admin/" exists
  And I turn on projectile-rails-mode
  And I open the app file "app/controllers/admin/foos_controller.rb"
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  class Admin::FoosController < ApplicationController

  end
  """

Scenario: Opening a new job
  When I turn on projectile-rails-mode
  And I open the app file "app/jobs/foo_job.rb"
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  class FooJob < ApplicationJob

  end
  """

Scenario: Opening a new lib
  When I turn on projectile-rails-mode
  And I open the app file "lib/fooing.rb"
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  module Fooing

  end
  """

Scenario: Opening a new file under the app directory
  When I turn on projectile-rails-mode
  And I open the app file "app/jobs/admin/foo_job.rb"
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  class Admin::FooJob < ApplicationJob

  end
  """

Scenario: Opening a new namespaced lib
  When I turn on projectile-rails-mode
  And directory "lib/admin/" exists
  When I open the app file "lib/admin/fooing.rb"
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  module Admin
    module Fooing

    end
  end
  """

Scenario: Opening a new spec
  When I turn on projectile-rails-mode
  And I open the app file "spec/models/bar_spec.rb"
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  require 'rails_helper'

  RSpec.describe Bar do

  end
  """

Scenario: Opening a new concern
  When I turn on projectile-rails-mode
  And I open the app file "app/models/concerns/foo.rb"
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  module Foo
    extend ActiveSupport::Concern

  end
  """

Scenario: Opening a new helper
  When I turn on projectile-rails-mode
  And I open the app file "app/helpers/foo_helper.rb"
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  module FooHelper

  end
  """

Scenario: Disabling the feature
  When I turn on projectile-rails-mode
  And I turn off snippet expansion
  When I open the app file "spec/models/bar_spec.rb"
  Then I should not see:
  """
  # frozen_string_literal: true

  require "spec_helper"
  """
