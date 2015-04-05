defmodule XBoard do
  defstruct [board: []]

  def handle(state = %XBoard{board: board}, "MOVE " <> move_string) do
    {:ok, move} = Move.parse(move_string)

    case Board.review_move(:white, move, board) do
      :ok ->
        new_board = Board.move(board, move)
        %{state | board: new_board}
      # {:error, error} ->
      #   IO.puts error_message(error)
      #   prompt_for_move(board, color, auto_sides)
    end
  end

  def handle(state, "new") do
    %{state | board: Board.new}
  end

  def handle(state, "quit") do
    # TODO: Terminate process
    %{state | board: []}
  end

  def handle(state, _command) do
    state
  end
end
