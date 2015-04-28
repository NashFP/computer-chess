defmodule Game do
  defp apply_move(board, color, move, auto_sides) do
    new_board = board |> Board.move(move)
    take_turn(new_board, Board.other_color(color), auto_sides)
  end

  def auto do
    start_with_automated([:white, :black])
  end

  defp error_message(:no_piece), do: "That square is empty."
  defp error_message(:opponent_piece), do: "That's not your piece."
  defp error_message(:illegal), do: "That piece can't do that."
  defp error_message(:self_check), do: "That would leave you in check."
  defp error_message(_), do: "Unknown error."

  defp get_best_move(color, board) do
    spawn(Game, :get_best_move_process, [color, board, self])
    receive do
      {:ok, move} -> move
    end
  end

  def get_best_move_process(color, board, pid) do
    move = Board.get_best_move_for_color(color, board)
    send(pid, {:ok, move})
  end

  defp get_move(board, color, auto_sides) do
    case is_computer?(color, auto_sides) do
      true ->
        move = get_best_move(color, board)
        apply_move(board, color, move, auto_sides)
      false ->
        prompt_for_move(board, color, auto_sides)
    end
  end

  defp is_computer?(color, auto_sides), do: Enum.any?(auto_sides, &(&1 == color))

  def main(args) do
    case args do
      ["auto"|_] -> auto
      ["text"|_] -> start
      _          -> XBoard.start
    end
  end

  defp prompt_for_move(board, color, auto_sides) do
    input = IO.gets("Please enter your move: >") |> String.rstrip

    case Move.parse(input) do
      {:error, _} ->
        IO.puts "Enter your move as [rank][file][rank][file], e.g., a2a4."
        prompt_for_move(board, color, auto_sides)
      {:ok, move} ->
        case Board.review_move(:white, move, board) do
          :ok ->
            apply_move(board, color, move, auto_sides)
          {:error, error} ->
            IO.puts error_message(error)
            prompt_for_move(board, color, auto_sides)
        end
    end
  end

  defp show_board(board) do
    board |> Board.to_string |> IO.puts
  end

  def start do
    start_with_automated([:black])
  end

  defp start_with_automated(auto_sides) do
    :random.seed :erlang.now
    take_turn(Board.new, :white, auto_sides)
  end

  defp take_turn(board, color, auto_sides) do
    show_board(board)

    case Board.can_move?(board, color) do
      true  ->
        if Board.in_check?(color, board), do: IO.puts "Check!"
        get_move(board, color, auto_sides)
      false ->
        IO.puts "Check and mate!"
        IO.puts "Game over!"
    end
  end
end
