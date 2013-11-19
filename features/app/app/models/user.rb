class User < ActiveRecord::Base
  validates_presence_of :id
  alias_attribute :to_s, :to_string
end
