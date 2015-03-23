defmodule Piece do
  defstruct color: nil, type: nil, square: nil

  defp all_directions do
    (for x <- -1..1, y <- -1..1, do: {x, y})
    |> Enum.reject(&(&1 == {0, 0}))
  end

  def forward(:black), do: -1
  def forward(:white), do: 1

  def get_move_length(:R), do: :long
  def get_move_length(:N), do: :short
  def get_move_length(:B), do: :long
  def get_move_length(:Q), do: :long
  def get_move_length(:K), do: :short

  def get_moves(%Piece{type: :R}) do
    for x <- [0, 1], s <- [1, -1], do: {x * s, (1 - x) * s}
  end

  def get_moves(%Piece{type: :N}) do
    for x <- [1, 2], y <- [1, -1], z <- [1, -1], do: {x * y, (3 - x) * z}
  end

  def get_moves(%Piece{type: :B}) do
    for x <- [1, -1], y <- [1, -1], do: {x, y}
  end

  def get_moves(%Piece{type: :Q}) do
    all_directions
  end

  def get_moves(%Piece{type: :K}) do
    all_directions
  end

  def parse(string) do
    [[_, color, type, file_rank]] = Regex.scan ~r/([BW])(.)([a-h][1-8])/, string

    %Piece{
      color: parse_color(color),
      type: String.to_existing_atom(type),
      square: Square.parse(file_rank)
    }
  end

  defp parse_color("B"), do: :black
  defp parse_color("W"), do: :white

  def piece_to_string(nil), do: ".."
  def piece_to_string(%Piece{color: :black, type: :P}), do: "♙ "
  def piece_to_string(%Piece{color: :black, type: :R}), do: "♖ "
  def piece_to_string(%Piece{color: :black, type: :N}), do: "♘ "
  def piece_to_string(%Piece{color: :black, type: :B}), do: "♗ "
  def piece_to_string(%Piece{color: :black, type: :Q}), do: "♕ "
  def piece_to_string(%Piece{color: :black, type: :K}), do: "♔ "
  def piece_to_string(%Piece{color: :white, type: :P}), do: "♟ "
  def piece_to_string(%Piece{color: :white, type: :R}), do: "♜ "
  def piece_to_string(%Piece{color: :white, type: :N}), do: "♞ "
  def piece_to_string(%Piece{color: :white, type: :B}), do: "♝ "
  def piece_to_string(%Piece{color: :white, type: :Q}), do: "♛ "
  def piece_to_string(%Piece{color: :white, type: :K}), do: "♚ "
end
