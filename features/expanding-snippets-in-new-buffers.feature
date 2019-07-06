Feature: Filling new buffer with class definition
  In order to work faster
  As a user
  I want to be able to create a new file and have the buffer filled a class skeleton

Background:
  Given I turn on snippet expansion

  @pending
Scenario: Opening new model
  When I open the app file "app/models/foo.rb"
  And I turn on projectile-rails-mode
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  class Foo < ActiveRecord::Base

  end
  """

  @pending
Scenario: Opening new model when ApplicationRecord exists
  Given file "app/models/application_record.rb" exists
  When I open the app file "app/models/foo.rb"
  And I turn on projectile-rails-mode
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  class Foo < ApplicationRecord

  end
  """

  @pending
Scenario: Opening new controller
  When I open the app file "app/controllers/foos_controller.rb"
  And I turn on projectile-rails-mode
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  class FoosController < ApplicationController

  end
  """

  @pending
Scenario: Opening a new namespaced controller
  Given directory "app/controllers/admin/" exists
  When I open the app file "app/controllers/admin/foos_controller.rb"
  And I turn on projectile-rails-mode
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  class Admin::FoosController < ApplicationController

  end
  """

  @pending
Scenario: Opening a new job
  When I open the app file "app/jobs/foo_job.rb"
  And I turn on projectile-rails-mode
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  class FooJob < ApplicationJob

  end
  """

  @pending
Scenario: Opening a new lib
  When I open the app file "lib/fooing.rb"
  And I turn on projectile-rails-mode
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  module Fooing

  end
  """

  @pending
Scenario: Opening a new file under the app directory
  When I open the app file "app/jobs/admin/foo_job.rb"
  And I turn on projectile-rails-mode
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  module Admin
    class FooJob

    end
  end
  """

  @pending
Scenario: Opening a new namespaced lib
  Given directory "lib/admin/" exists
  When I open the app file "lib/admin/fooing.rb"
  And I turn on projectile-rails-mode
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  module Admin
    module Fooing

    end
  end
  """

  @pending
Scenario: Opening a new spec
  When I open the app file "spec/models/bar_spec.rb"
  And I turn on projectile-rails-mode
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  require "rails_helper"

  RSpec.describe Bar do

  end
  """

  @pending
Scenario: Opening a new concern
  When I open the app file "app/models/concerns/foo.rb"
  And I turn on projectile-rails-mode
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  module Foo
    extend ActiveSupport::Concern

  end
  """

  @pending
Scenario: Opening a new helper
  When I open the app file "app/helpers/foo_helper.rb"
  And I turn on projectile-rails-mode
  Then disregarding whitespaces I should see:
  """
  # frozen_string_literal: true

  module FooHelper

  end
  """

  @pending
Scenario: Opening a new spec and the buffer is not empty
  When I open the app file "spec/models/bar_spec.rb"
  And I insert:
  """
  Emacs!
  """
  And I turn on projectile-rails-mode
  Then I should not see:
  """
  # frozen_string_literal: true

  require "spec_helper"
  """

  @pending
Scenario: Disabling the feature
  Given I turn off snippet expansion
  When I open the app file "spec/models/bar_spec.rb"
  And I turn on projectile-rails-mode
  Then I should not see:
  """
  # frozen_string_literal: true

  require "spec_helper"
  """
