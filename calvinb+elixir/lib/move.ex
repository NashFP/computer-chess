defmodule Move do
  defstruct from: nil, to: nil

  def parse(string) do
    case Regex.match?(~r/^[a-h][1-8][a-h][1-8]$/, string) do
      true -> 
        move = %Move{
          from: string |> String.slice(0, 2) |> Square.parse,
          to:   string |> String.slice(2, 2) |> Square.parse
        }
        {:ok, move}
      false -> {:error, :invalid_format}
    end
  end

  def to_string(%Move{from: from, to: to}) do
    Square.to_string(from) <> Square.to_string(to)
  end
end
