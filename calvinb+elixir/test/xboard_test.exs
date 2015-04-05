defmodule XBoardTest do
  use ExUnit.Case

  test "should handle xboard command" do
    state = %XBoard{}
    new_state = XBoard.handle(state, "xboard")
    assert new_state === state
  end

  test "should handle protover command" do
    state = %XBoard{}
    new_state = XBoard.handle(state, "protover 2")
    assert new_state === state
  end

  test "should handle new command" do
    state = %XBoard{}
    new_state = XBoard.handle(state, "new")
    assert new_state.board === Board.new
  end

  test "should handle quit" do
    state = %XBoard{}
    |> XBoard.handle("new")
    |> XBoard.handle("quit")
    assert state.board === []
  end
end
