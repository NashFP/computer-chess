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
    assert response === {state, "feature usermove=1 done=1"}
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
    |> XBoard.handle("usermove e2e4")
    assert state.board == Board.parse("WPe4+")
  end

  test "should handle illegal move" do
    state = %XBoard{board: Board.parse("WPe2")}
    response = state |> XBoard.handle("usermove e2e5")
    assert response === {state, "Illegal move (illegal): e2e5"}
  end

  test "should handle move parse failure" do
    state = %XBoard{board: Board.parse("WPe2")}
    response = state |> XBoard.handle("usermove crap")
    assert response === {state, "Illegal move (invalid_format): crap"}
  end

  test "should report move" do
    move_string = "e2e4"
    {:ok, move} = Move.parse(move_string)
    message = XBoard.report_move(move)
    assert message === "move " <> move_string
  end
end
