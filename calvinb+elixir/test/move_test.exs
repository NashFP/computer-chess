defmodule MoveTest do
  use ExUnit.Case

  test "should handle parsing of bad string" do
    move = Move.parse("crap")
    assert move == {:error, :invalid_format}
  end
end
