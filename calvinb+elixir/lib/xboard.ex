defmodule XBoard do
  defstruct [board: []]

  def handle(state = %XBoard{board: board}, "usermove " <> move_string) do
    case Move.parse(move_string) do
      {:ok, move} ->
        case Board.review_move(:white, move, board) do
          :ok ->
            new_board = Board.move(board, move)
            %{state | board: new_board}
          {:error, error} ->
            {state, "Illegal move (" <> to_string(error) <> "): " <> move_string}
        end
      {:error, error} ->
        {state, "Illegal move (" <> to_string(error) <> "): " <> move_string}
    end
  end

  def handle(state, "new") do
    %{state | board: Board.new}
  end

  def handle(state, "protover " <> _version) do
    {state, "feature usermove=1 done=1"}
  end

  def handle(state, "quit") do
    # TODO: Terminate process
    %{state | board: []}
  end

  def handle(state, _command) do
    state
  end

  def report_move(move) do
    "move " <> Move.to_string(move)
  end
end
