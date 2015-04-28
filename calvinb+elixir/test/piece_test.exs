defmodule PieceTest do
  use ExUnit.Case

  test "should initially have has_moved == false" do
    piece = Piece.parse("WRa1")
    assert piece.has_moved == false
  end
end
