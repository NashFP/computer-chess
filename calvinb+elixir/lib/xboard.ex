defmodule XBoard do
  defstruct [board: []]

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
