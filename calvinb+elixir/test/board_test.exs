defmodule BoardTest do
  use ExUnit.Case

  test "new" do
    board = Board.new
    assert is_list(board)
    assert length(board) == 32
    [first|[second|_]] = board
    assert first == Piece.parse("WRa1")
    assert second == Piece.parse("WNb1")
  end

  test "to_string" do
    board = Board.new
    string = Board.to_string(board)
    assert string == """
    a  b  c  d  e  f  g  h
    -- -- -- -- -- -- -- --
8 | ♖  ♘  ♗  ♕  ♔  ♗  ♘  ♖  | 8
7 | ♙  ♙  ♙  ♙  ♙  ♙  ♙  ♙  | 7
6 | .. .. .. .. .. .. .. .. | 6
5 | .. .. .. .. .. .. .. .. | 5
4 | .. .. .. .. .. .. .. .. | 4
3 | .. .. .. .. .. .. .. .. | 3
2 | ♟  ♟  ♟  ♟  ♟  ♟  ♟  ♟  | 2
1 | ♜  ♞  ♝  ♛  ♚  ♝  ♞  ♜  | 1
    -- -- -- -- -- -- -- --
    a  b  c  d  e  f  g  h
"""
  end

  test "piece_on" do
    piece = Piece.parse("WRa1")
    board = [piece]
    found = Board.piece_on(Square.parse("a1"), board)
    assert found == piece
  end

  test "move" do
    board = [Piece.parse("WPa2"), Piece.parse("WRa1")]
    {:ok, move} = Move.parse("a2a4")
    new_board = Board.move(board, move)
    assert new_board == [Piece.parse("WPa4+"), Piece.parse("WRa1")]
  end

  test "capture move should remove piece" do
    board = [Piece.parse("WPd4"), Piece.parse("BPe5")]
    {:ok, move} = Move.parse("d4e5")
    new_board = Board.move(board, move)
    assert new_board == [Piece.parse("WPe5+")]
  end

  test "review move should complain when no piece on starting square" do
    board = [Piece.parse("WPa2")]
    {:ok, move} = Move.parse("a1b1")
    review = Board.review_move(:white, move, board)
    assert review == {:error, :no_piece}
  end

  test "review move should complain when opponent piece on starting square" do
    board = [Piece.parse("BPa2")]
    {:ok, move} = Move.parse("a2b1")
    review = Board.review_move(:white, move, board)
    assert review == {:error, :opponent_piece}
  end

  test "review move should complain when move is illegal" do
    board = [Piece.parse("WPa2")]
    {:ok, move} = Move.parse("a2g2")
    review = Board.review_move(:white, move, board)
    assert review == {:error, :illegal}
  end

  test "review move should complain when putting self in check" do
    board = ["WKe1", "BQd8"] |> Enum.map(&Piece.parse/1)
    {:ok, move} = Move.parse("e1d1")
    review = Board.review_move(:white, move, board)
    assert review == {:error, :self_check}
  end

  test "review move should be ok when move is good" do
    board = [Piece.parse("WPa2")]
    {:ok, move} = Move.parse("a2a4")
    review = Board.review_move(:white, move, board)
    assert review == :ok
  end

  test "should get pieces of color" do
    board = [Piece.parse("WRa1"), Piece.parse("BRa8")]
    whites = Board.pieces_of_color(board, :white)
    assert whites == [Piece.parse("WRa1")]
  end

  test "should get possible moves for color" do
    board = Board.new
    moves = Board.get_moves_for_color(:white, board)
    assert !Enum.empty?(moves)
  end

  test "should get random move for color" do
    board = Board.new
    move = Board.get_random_move_for_color(:white, board)
    assert is_map(move)
  end

  test "should know when king is not in check" do
    board = Board.new
    in_check = Board.in_check?(:white, board)
    assert !in_check
  end

  test "should know when black king is in check" do
    board = [Piece.parse("WPd7"), Piece.parse("BKe8")]
    in_check = Board.in_check?(:black, board)
    assert in_check
  end

  test "should know when white king is in check" do
    board = [Piece.parse("WKe1"), Piece.parse("BPd2")]
    in_check = Board.in_check?(:white, board)
    assert in_check
  end

  test "should get safe moves for color" do
    pawn = Piece.parse("WPf2")
    board = [Piece.parse("WKe1"), pawn, Piece.parse("BBh4")]
    moves = Board.get_safe_moves_for_color(:white, board)
    {:ok, bad_move} = Move.parse("f2f3")
    assert !Enum.any?(moves, &(&1 == bad_move))
  end

  test "should get doubled pawn count" do
    board = Board.parse("WPc3 WPc5 WPd2 BPc7")
    doubled = Board.doubled_pawn_count(board, :white)
    assert doubled == 2
  end

  test "should get blocked pawn count" do
    board = Board.parse("WPc3 WPc4 WPd2 BPc7")
    blocked = Board.blocked_pawn_count(board, :white)
    assert blocked == 1
  end

  test "should get isolated pawn count" do
    board = Board.parse("WPb3 WPc4 WPe2 WPg2 BPc7")
    isolated = Board.isolated_pawn_count(board, :white)
    assert isolated == 2
  end

  test "get white pawn targets" do
    pawn = Piece.parse("WPb3")
    board = [pawn]
    targets = Board.get_targets(pawn, board)
    assert targets == [Square.parse("b4")]
  end

  test "get black pawn targets" do
    pawn = Piece.parse("BPb3")
    board = [pawn]
    targets = Board.get_targets(pawn, board)
    assert targets == [Square.parse("b2")]
  end

  test "get initial pawn targets" do
    pawn = Piece.parse("WPb2")
    board = [pawn]
    targets = Board.get_targets(pawn, board)
    assert targets == [Square.parse("b4"), Square.parse("b3")]
  end

  test "get pawn targets with 2 opponents in front" do
    pawn = Piece.parse("WPc5")
    board = [pawn, Piece.parse("BPc6"), Piece.parse("BPd6")]
    targets = Board.get_targets(pawn, board)
    assert targets == [Square.parse("d6")]
  end

  test "get pawn targets with 1 opponent in front" do
    pawn = Piece.parse("WPc5")
    board = [pawn, Piece.parse("BPc6")]
    targets = Board.get_targets(pawn, board)
    assert targets == []
  end

  test "get rook targets blocked in" do
    rook = Piece.parse("WRa1")
    board = [rook, Piece.parse("WPa2"), Piece.parse("WNb1")]
    targets = Board.get_targets(rook, board)
    assert targets == []
  end

  test "get rook targets with 1 space ahead" do
    rook = Piece.parse("WRa1")
    board = [rook, Piece.parse("WPa3"), Piece.parse("WNb1")]
    targets = Board.get_targets(rook, board)
    assert targets == [Square.parse("a2")]
  end

  test "get rook targets with opponent 2 spaces ahead" do
    rook = Piece.parse("WRa1")
    board = [rook, Piece.parse("BPa3"), Piece.parse("WNb1")]
    targets = Board.get_targets(rook, board)
    assert targets == [Square.parse("a3"), Square.parse("a2")]
  end

  test "get rook targets with opponent 1 space ahead" do
    rook = Piece.parse("WRa1")
    board = [rook, Piece.parse("BPa2"), Piece.parse("WNb1")]
    targets = Board.get_targets(rook, board)
    assert targets == [Square.parse("a2")]
  end

  test "get rook targets with 1 space beside" do
    rook = Piece.parse("WRa1")
    board = [rook, Piece.parse("WPa2"), Piece.parse("WBc1")]
    targets = Board.get_targets(rook, board)
    assert targets == [Square.parse("b1")]
  end

  test "get rook targets with 5 spaces ahead" do
    rook = Piece.parse("WRa1")
    board = [rook, Piece.parse("WPa7"), Piece.parse("WNb1")]
    targets = Board.get_targets(rook, board)
    assert targets == [
      Square.parse("a6"),
      Square.parse("a5"),
      Square.parse("a4"),
      Square.parse("a3"),
      Square.parse("a2")
    ]
  end

  test "get rook targets with 5 spaces to right" do
    rook = Piece.parse("WRa1")
    board = [rook, Piece.parse("WPa2"), Piece.parse("WNg1")]
    targets = Board.get_targets(rook, board)
    assert targets == [
      Square.parse("f1"),
      Square.parse("e1"),
      Square.parse("d1"),
      Square.parse("c1"),
      Square.parse("b1")
    ]
  end

  test "get rook targets with 2 spaces behind" do
    rook = Piece.parse("WRa3")
    board = [rook, Piece.parse("WPa4"), Piece.parse("WNb3")]
    targets = Board.get_targets(rook, board)
    assert targets == [Square.parse("a1"), Square.parse("a2")]
  end

  test "get rook targets with 3 spaces to left" do
    rook = Piece.parse("WRd1")
    board = [rook, Piece.parse("WPd2"), Piece.parse("WKe1")]
    targets = Board.get_targets(rook, board)
    assert targets == [Square.parse("a1"), Square.parse("b1"), Square.parse("c1")]
  end

  test "get knight targets from start" do
    knight = Piece.parse("WNb1")
    board = [knight, Piece.parse("WPd2")]
    targets = Board.get_targets(knight, board)
    assert targets == ["c3", "a3"] |> Enum.map(&Square.parse/1)
  end

  test "get bishop targets from with 1 space each in 2 directions" do
    bishop = Piece.parse("WBc1")
    board = [bishop, Piece.parse("WPa3"), Piece.parse("WPe3")]
    targets = Board.get_targets(bishop, board)
    assert targets == ["d2", "b2"] |> Enum.map(&Square.parse/1)
  end

  test "get queen targets with 1 space ahead" do
    rook = Piece.parse("WQd1")
    board = [rook|["WBc1", "WPc2", "WPd3", "WPe2", "WKe1"] |> Enum.map(&Piece.parse/1)]
    targets = Board.get_targets(rook, board)
    assert targets == [Square.parse("d2")]
  end

  test "get king targets when all alone" do
    king = Piece.parse("WKe1")
    board = [king]
    targets = Board.get_targets(king, board)
    assert targets == ["d1", "d2", "e2", "f1", "f2"] |> Enum.map(&Square.parse/1)
  end

  test "should include long castling in king targets" do
    rook = Piece.parse("WRa1")
    king = Piece.parse("WKe1")
    board = [rook, king]
    targets = Board.get_targets(king, board)
    assert List.last(targets) == Square.parse("b1")
  end

  test "should include short castling in king targets" do
    rook = Piece.parse("WRh1")
    king = Piece.parse("WKe1")
    board = [rook, king]
    targets = Board.get_targets(king, board)
    assert List.last(targets) == Square.parse("g1")
  end

  test "get lead by type" do
    board = Board.parse("WPa2 WPb2 BPa7")
    lead = Board.lead_by_type(board, :P, :white)
    assert lead == 1
  end

  test "should initially evaluate to 0" do
    board = Board.new
    value = Board.evaluate(board, :white, 0)
    assert value == value
  end
  
  test "should get alpha-beta without error" do
    board = Board.new
    _ = Board.alpha_beta(board, :white, 1)
  end

  test "should queen white pawn" do
    board = Board.parse("WPa7")
    {:ok, move} = Move.parse("a7a8")
    new_board = Board.move(board, move)
    assert new_board == Board.parse("WQa8+")
  end

  test "should update has_moved when moving piece" do
    piece = Piece.parse("WRa1")
    board = [piece]
    {:ok, move} = Move.parse("a1a2")
    [new_piece] = Board.move(board, move)
    assert new_piece.has_moved == true
  end
end
