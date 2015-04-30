defmodule Square do
  defstruct file: nil, rank: nil

  def add_file(file, dx) do
    case file
    |> to_char_list
    |> Enum.map(&(&1 + dx))
    |> Enum.filter(&(&1 >= ?a && &1 <= ?h)) do
      ''        -> nil
      char_list -> char_list |> List.to_existing_atom
    end
  end

  def file_diff(square1, square2) do
    int1 = square1.file |> to_char_list |> List.first
    int2 = square2.file |> to_char_list |> List.first
    abs(int1 - int2)
  end

  def move(nil, _), do: nil

  def move(square = %Square{}, {dx, dy}) do
    case {add_file(square.file, dx), square.rank + dy} do
      {nil, _}                            -> nil
      {_, rank} when rank < 1 or rank > 8 -> nil
      {file, rank}                        -> %{square | file: file, rank: rank}
    end
  end

  def new(file, rank) do
    %Square{file: file, rank: rank}
  end
  
  def parse(string) do
    %Square{
      file: string |> String.slice(0, 1) |> String.to_existing_atom,
      rank: string |> String.slice(1, 1) |> String.to_integer
    }
  end

  def to_string(%Square{file: file, rank: rank}) do
    "#{file}#{rank}"
  end
end
