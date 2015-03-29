defmodule Board do
  def alpha_beta(board, color, depth), do: alpha_beta(board, color, -999999, 999999, depth)

  defp alpha_beta(board, color, _, _, 0), do: evaluate(board, color)

  defp alpha_beta(board, color, alpha, beta, depth_left) do
    moves = get_safe_moves_for_color(color, board)

    {final_alpha, cut_off} = List.foldl(moves, {alpha, false}, fn(m, {cur_alpha, cutting_off}) ->
      case cutting_off do
        true  -> {cur_alpha, true}
        false ->
          new_board = move(board, m)
          score = -alpha_beta(new_board, other_color(color), -beta, -cur_alpha, depth_left - 1)
          cond do
            score >= beta     -> {cur_alpha, true}
            score > cur_alpha -> {score, false}
            true              -> {cur_alpha, false}
          end
      end
    end)

    case cut_off do
      true  -> beta
      false -> final_alpha
    end
  end

  def blocked_pawn_count(board, color) do
    board |> Enum.filter(&(&1.type == :P and &1.color == color))
    |> Enum.filter(&pawn_blocked?(board, &1))
    |> length
  end

  def can_move?(board, color) do
    get_safe_moves_for_color(color, board) != []
  end

  defp compress(list), do: list |> Enum.reject(&(&1 == nil))

  defp create_back_row(color, rank) do
    [
      %Piece{color: color, type: :R, square: Square.new(:a, rank)},
      %Piece{color: color, type: :N, square: Square.new(:b, rank)},
      %Piece{color: color, type: :B, square: Square.new(:c, rank)},
      %Piece{color: color, type: :Q, square: Square.new(:d, rank)},
      %Piece{color: color, type: :K, square: Square.new(:e, rank)},
      %Piece{color: color, type: :B, square: Square.new(:f, rank)},
      %Piece{color: color, type: :N, square: Square.new(:g, rank)},
      %Piece{color: color, type: :R, square: Square.new(:h, rank)}
    ]
  end

  defp create_pawns(color, rank) do
    files
    |> Enum.map &%Piece{color: color, type: :P, square: Square.new(&1, rank)}
  end

  defp create_pieces(color, back_rank, front_rank) do
    Enum.concat create_back_row(color, back_rank), create_pawns(color, front_rank)
  end

  def doubled_pawn_count(board, color) do
    doubled_pawns_in_file = fn(file) ->
      board
      |> Enum.filter(fn(piece) -> 
           piece.type == :P and piece.color == color and piece.square.file == file 
         end)
      |> length
    end

    files
    |> Enum.map(doubled_pawns_in_file)
    |> Enum.filter(&(&1 > 1))
    |> Enum.sum
  end

  def evaluate(board, color) do
    lead_by_pieces = [
      {:K, 200},
      {:Q, 9},
      {:R, 5},
      {:B, 3},
      {:N, 3},
      {:P, 1}
    ]
    |> Enum.map(fn({type, factor}) -> lead_by_type(board, type, color) * factor end)
    |> Enum.sum

    lead_by_pawns =
    [&doubled_pawn_count/2, &blocked_pawn_count/2, &isolated_pawn_count/2]
    |> Enum.map(&(&1.(board, color) - &1.(board, other_color(color))))
    |> Enum.sum

    [ours, theirs] = [color, other_color(color)]
    |> Enum.map(&length(get_safe_moves_for_color(&1, board)))

    lead_by_mobility = ours - theirs

    lead_by_pieces - 0.5 * lead_by_pawns + 0.1 * lead_by_mobility
  end

  defp files do
    ?a..?h
    |> Enum.map(&List.to_existing_atom([&1]))
  end

  defp find_king(color, board) do
    board
    |> Enum.drop_while(&(&1.color != color || &1.type != :K))
    |> List.first
  end

  defp finish_castling(board, king = %Piece{square: %Square{file: :b}}) do
    move(board, %Move{from: %Square{file: :a, rank: king.square.rank}, 
                      to:   %Square{file: :c, rank: king.square.rank}})
  end

  defp finish_castling(board, king = %Piece{square: %Square{file: :g}}) do
    move(board, %Move{from: %Square{file: :h, rank: king.square.rank}, 
                      to:   %Square{file: :f, rank: king.square.rank}})
  end

  def get_best_move_for_color(color, board) do
    search_root(board, color, 2)
  end

  defp get_castling_targets(board, king = %Piece{type: :K, has_moved: false}) do
    Enum.concat(maybe_castle_left(board, king), maybe_castle_right(board, king))
  end

  defp get_castling_targets(_, _), do: []

  defp get_long_targets(directions, piece, board) do
    directions
    |> Enum.map(&long_move(piece.square, piece.color, board, &1))
    |> Enum.concat
    |> compress
  end

  def get_random_move_for_color(color, board) do
    case get_safe_moves_for_color(color, board) do
      []     -> {:error, :checkmate}
      [move] -> move
      moves  ->
        index = :random.uniform(length(moves)) - 1
        Enum.at(moves, index)
    end
  end

  def get_moves_for_color(color, board) do
    pieces_of_color(board, color)
    |> Enum.map(&get_moves_for_piece(&1, board))
    |> Enum.concat
  end

  defp get_moves_for_piece(piece, board) do
    get_targets(piece, board)
    |> Enum.map(&(%Move{from: piece.square, to: &1}))
  end

  defp get_piece(pieces, file, rank) do
    square = %Square{file: file, rank: rank}
    pieces
    |> Enum.drop_while(fn(piece) -> piece.square != square end)
    |> List.first
  end

  def get_safe_moves_for_color(color, board) do
    bad_move? = fn(m) ->
      new_board = Board.move(board, m)
      in_check?(color, new_board)
    end

    get_moves_for_color(color, board)
    |> Enum.reject(bad_move?)
  end

  defp get_short_targets(steps, piece, board) do
    steps
    |> Enum.map(&Square.move(piece.square, &1))
    |> compress
    |> reject_friends(piece, board)
  end

  def get_targets(piece = %Piece{type: :P}, board) do
    dy = Piece.forward(piece.color)

    [
      piece.square |> Square.move({0, dy})  |> if_empty(board)
                   |> Square.move({0, dy})  |> if_empty(board) |> if_first_pawn_move(piece),
      piece.square |> Square.move({0, dy})  |> if_empty(board),
      piece.square |> Square.move({-1, dy}) |> if_opponent(piece.color, board),
      piece.square |> Square.move({1, dy})  |> if_opponent(piece.color, board)
    ] |> compress
  end

  def get_targets(piece = %Piece{}, board) do
    f = case Piece.get_move_length(piece.type) do
      :short -> &get_short_targets/3
      :long  -> &get_long_targets/3
    end

    Piece.get_moves(piece)
    |> f.(piece, board)
    |> Enum.concat(get_castling_targets(board, piece))
  end

  def has_color_at?(square, color, board) do
    case piece_on(square, board) do
      nil         -> false
      other_piece -> other_piece.color == color
    end
  end

  defp has_unmoved_rook?(board, color, file) do
    is_unmoved_rook? = fn(piece) ->
      case piece do
        %Piece{type: :R, color: ^color, square: %Square{file: ^file}, has_moved: false} -> true
        _ -> false
      end
    end
    Enum.any?(board, is_unmoved_rook?)
  end

  # No ? on name because it's not a predicate; returns Square or nil, not Boolean
  defp if_empty(nil, _), do: nil

  defp if_empty(square, board) do
    case piece_on(square, board) do
      nil -> square
      _   -> nil
    end
  end

  defp if_first_pawn_move(nil, _), do: nil

  defp if_first_pawn_move(square, piece) do
    case piece.square.rank == starting_pawn_rank(piece.color) do
      true -> square
      _    -> nil
    end
  end

  defp if_opponent(nil, _, _), do: nil

  defp if_opponent(square = %Square{}, color, board) do
    case Board.piece_on(square, board) do
      nil                   -> nil
      %Piece{color: ^color} -> nil
      _                     -> square
    end
  end

  def in_check?(color, board) do
    case find_king(color, board) do
      nil  -> false
      king ->
        get_moves_for_color(other_color(color), board)
        |> Enum.any?(&(&1.to == king.square))
    end
  end

  defp isolated?(pawn, board) do
    has_friendly_pawn? = fn(file) ->
      case file do
        nil -> false
        _   ->
          board
          |> Enum.any?(&(&1.type == :P and &1.color == pawn.color and &1.square.file == file))
      end
    end

    has_friend = [-1, 1]
    |> Enum.map(&Square.add_file(pawn.square.file, &1))
    |> Enum.any?(has_friendly_pawn?)

    !has_friend
  end

  def isolated_pawn_count(board, color) do
    board
    |> Enum.filter(fn(p) -> p.type == :P and p.color == color end)
    |> Enum.filter(&isolated?(&1, board))
    |> length
  end

  def lead_by_type(board, type, color) do
    {ours, theirs} = board
    |> Enum.filter(&(&1.type == type))
    |> Enum.partition(&(&1.color == color))

    length(ours) - length(theirs)
  end

  defp long_move(square, color, board, next) do
    long_move(square, color, board, next, [])
  end

  defp long_move(nil, _, _, _, result), do: result

  defp long_move(square, color, board, next, result) do
    next_square = Square.move(square, next)

    case piece_on(next_square, board) do
      %Piece{color: ^color} -> result
      nil                   -> long_move(next_square, color, board, next, [next_square|result])
      _                     -> [next_square|result]
    end
  end

  defp maybe_castle_left(board, king = %Piece{type: :K, has_moved: false}) do
    spot_empty? = fn(file) ->
      piece_on(%Square{file: file, rank: king.square.rank}, board) == nil
    end

    path_clear = Enum.all?([:b, :c, :d], spot_empty?)
    has_rook = has_unmoved_rook?(board, king.color, :a)

    case path_clear and has_rook do
      true -> [%Square{file: :b, rank: king.square.rank}]
      false -> []
    end
  end

  defp maybe_castle_right(board, king = %Piece{type: :K, has_moved: false}) do
    spot_empty? = fn(file) ->
      piece_on(%Square{file: file, rank: king.square.rank}, board) == nil
    end

    path_clear = Enum.all?([:f, :g], spot_empty?)

    case path_clear and has_unmoved_rook?(board, king.color, :h) do
      true -> [%Square{file: :g, rank: king.square.rank}]
      false -> []
    end
  end

  defp maybe_finish_castling(board, from_to) do
    case piece_on(from_to.to, board) do
      king = %Piece{type: :K} ->
        case Square.file_diff(from_to.from, from_to.to) > 1 do
          true  -> finish_castling(board, king)
          false -> board
        end
      _ -> board
    end
  end

  defp maybe_move_piece(piece, move) do
    case piece.square == move.from do
      true  -> move_piece_to(piece, move.to)
      false -> piece
    end
  end

  def move(board, from_to) do
    captured = piece_on(from_to.to, board)

    board
    |> Enum.reject(&(&1 == captured))
    |> Enum.map(&maybe_move_piece(&1, from_to))
    |> maybe_finish_castling(from_to)
  end

  defp move_piece_to(piece = %Piece{type: :P, color: :black}, square = %Square{rank: 1}) do
    %{piece | square: square, has_moved: true, type: :Q}
  end

  defp move_piece_to(piece = %Piece{type: :P, color: :white}, square = %Square{rank: 8}) do
    %{piece | square: square, has_moved: true, type: :Q}
  end

  defp move_piece_to(piece, square) do
    %{piece | square: square, has_moved: true}
  end

  def new do
    create_pieces(:white, 1, 2)
    |> Enum.concat(create_pieces(:black, 8, 7))
  end

  def other_color(:black), do: :white
  def other_color(:white), do: :black

  def parse(string) do
    string
    |> String.split
    |> Enum.map(&Piece.parse/1)
  end

  defp pawn_blocked?(board, pawn) do
    case Square.move(pawn.square, {0, Piece.forward(pawn.color)}) do
      nil    -> false
      square -> Board.piece_on(square, board) != nil
    end
  end

  def piece_on(square, board) do
    board
    |> Enum.drop_while(&(&1.square != square))
    |> List.first
  end

  def pieces_of_color(board, color) when is_atom(color) do
    board |> Enum.filter(fn(piece) -> piece.color == color end)
  end

  defp rank_to_string(pieces, rank) do
    squares = files
    |> Enum.map(&get_piece(pieces, &1, rank))
    |> Enum.map(&Piece.piece_to_string(&1))
    |> Enum.join(" ")

    [rank, squares, rank]
    |> Enum.join(" | ")
  end

  defp reject_friends(squares, piece, board) do
    squares
    |> Enum.reject(&has_color_at?(&1, piece.color, board))
  end

  def review_move(color, move, board) do
    case review_move_start(color, move, board) do
      :ok   -> review_move_legality(move, board)
      error -> error
    end
  end

  defp review_move_for_self_check(move, board) do
    piece = piece_on(move.from, board)
    new_board = Board.move(board, move)
    case in_check?(piece.color, new_board) do
      true  -> {:error, :self_check}
      false -> :ok
    end
  end

  defp review_move_legality(move, board) do
    piece = piece_on(move.from, board)
    targets = get_targets(piece, board)
    case Enum.any?(targets, &move.to == &1) do
      true -> review_move_for_self_check(move, board)
      _    -> {:error, :illegal}
    end
  end

  defp review_move_start(color, move, board) do
    case piece_on(move.from, board) do
      %Piece{color: ^color} -> :ok
      nil                   -> {:error, :no_piece}
      _                     -> {:error, :opponent_piece}
    end
  end

  defp search_root(board, color, depth) do
    moves = get_safe_moves_for_color(color, board)
    search_root(board, color, depth, moves)
  end

  defp search_root(board, color, depth, moves) do
    {alpha, beta} = {-999999, 999999}

    {_, _, final_scored_moves} =
      List.foldl(moves, {alpha, false, []},
        fn(m, {cur_alpha, cutting_off, scored_moves}) ->
          case cutting_off do
            true  -> {cur_alpha, true, [{m, -999999}|scored_moves]}
            false ->
              new_board = move(board, m)
              score = -alpha_beta(new_board, other_color(color), -beta, -cur_alpha, depth - 1)
              new_scored_moves = [{m, score}|scored_moves]
              cond do
                score >= beta     -> {cur_alpha, true, new_scored_moves}
                score > cur_alpha -> {score, false, new_scored_moves}
                true              -> {cur_alpha, false, new_scored_moves}
              end
          end
        end)

    {best_move, _} = final_scored_moves
    |> Enum.sort_by(fn({_, score}) -> score end)
    |> List.last

    best_move
  end

  defp starting_pawn_rank(:black), do: 7
  defp starting_pawn_rank(:white), do: 2

  def to_string(pieces) do
    files = "    a  b  c  d  e  f  g  h"
    edge  = "    -- -- -- -- -- -- -- --"

    result = 8..1
    |> Enum.map(&rank_to_string(pieces, &1))
    |> Enum.join("\n")

    [files, edge, result, edge, files, ""]
    |> Enum.join("\n")
  end

end
