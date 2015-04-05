defmodule XBoard do
  defstruct [board: []]

  def get_game_end_message(:black), do: "RESULT 0-1"
  def get_game_end_message(:white), do: "RESULT 1-0"

  def get_move_message(move) do
    "move " <> Move.to_string(move)
  end

  def handle(state, "new") do
    %{state | board: Board.new}
  end

  def handle(state, "protover " <> _version) do
    {state, "feature usermove=1 done=1"}
  end

  def handle(state, "quit") do
    :quit
  end

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

  def handle(state, _command) do
    state
  end

  defp loop(state) do
    command = IO.gets("") |> String.strip
    case handle(state, command) do
      :quit -> :ok
      {new_state, output} ->
        IO.puts output
        loop(new_state)
      new_state ->
        loop(new_state)
    end
  end

  def start do
    loop(%XBoard{})
  end
end
