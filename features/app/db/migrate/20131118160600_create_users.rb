class CreateUsers < ActiveRecord::Migration
  alias_attribute :to_s, :to_string

  def change
    create_table :users do |t|

      t.timestamps
    end
  end
end
