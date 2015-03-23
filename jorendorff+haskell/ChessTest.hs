import Chess
import ChessAI
import Minimax
import XBoard

assert True msg = putStrLn $ "PASS - " ++ msg
assert False msg = putStrLn $ "FAIL - " ++ msg

move str = (read str :: ChessMove)

assertEq actual expected msg =
  if actual == expected
  then putStrLn $ "PASS - " ++ msg
  else putStrLn $ "FAIL - " ++ msg ++ " (got " ++ show actual ++ ", expected " ++ show expected ++ ")"

assertLegalMove g moveStr msg =
  let actualMoves = moves g
      m = move moveStr
  in if elem m actualMoves
     then putStrLn $ "PASS - " ++ msg
     else putStrLn $ "FAIL - " ++ msg ++ " (got " ++ show actualMoves ++ ", expected " ++ show m ++ ")"

assertNothingAt board squareStr msg =
  let sq = charsToColRow squareStr
  in assertEq (getPieceAt board sq) Nothing msg

liftTestFromEither (Right msg) = putStrLn $ "PASS - "++ msg
liftTestFromEither (Left msg) = putStrLn $ "FAIL - " ++ msg

main = do
  mapM_ liftTestFromEither Chess.unitTests

  -- Consider a silly example of an en passant position (from a bad puzzle)
  do let board = fenToBoard "6R1/kp6/8/1KpP4/8/8/8/6B1 w - c6 0 36"
     assertEq (enPassant board) 0x0000040000000000 "parse en passant target square"
     assertLegalMove board "d5c6" "recognize en passant"
     let after = applyMove board (move "d5c6")
     assertNothingAt after "c5" "en passant removes enemy pawn from the board"

  -- A board can have two en passant captures possible
  do let board = fenToBoard "8/8/4k3/8/3pPp2/8/5K2/8 b - e3 0 57"
     assertLegalMove board "d4e3" "recognize black en passant toward king's side"
     assertLegalMove board "f4e3" "recognize black en passant toward queen's side"

  -- En passant can be the only way out of check and thus the only legal move
  do let board = fenToBoard "4k3/pp2pp2/2p2r2/3pP2b/1P1BKP2/2PQP3/P6P/3R4 w - d6 0 38"
     assertEq (moves board) [move "e5d6"] "en passant only way out of check"

  -- Require the AI to recognize a (spectacularly unlikely) checkmate by en passant
  do let board = fenToBoard "rnbq1b2/2kr1ppp/2n5/ppPQ4/8/4B3/5K2/8 w - b6 0 21"
     assertEq (chessAI board) (move "c5b6") "checkmate by en passant"
