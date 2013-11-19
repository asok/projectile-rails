class UsersController < ApplicationController
  before_filter :to_s
  alias_attribute :to_s, :to_string
end
