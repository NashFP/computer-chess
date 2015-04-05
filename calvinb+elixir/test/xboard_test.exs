defmodule XBoardTest do
  use ExUnit.Case

  test "should handle xboard command" do
    state = %XBoard{}
    new_state = XBoard.handle(state, "xboard")
    assert new_state === state
  end

  test "should send feature upon receiving protover command" do
    state = %XBoard{}
    response = XBoard.handle(state, "protover 2")
    assert response === {state, "feature"}
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

  test "should handle legal move" do
    state = %XBoard{board: Board.parse("WPe2")}
    |> XBoard.handle("MOVE e2e4")
    assert state.board == Board.parse("WPe4+")
  end
end
