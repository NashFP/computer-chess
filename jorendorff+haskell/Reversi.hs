{-# LANGUAGE TypeFamilies #-}

import Minimax
import Data.Array
import Vs
import Data.List(intersperse, intercalate, elemIndex)
import Data.Char(toLower)

--- Reversi

data Square = White | Black | Empty
  deriving Eq

flipSquare White = Black
flipSquare Black = White
flipSquare Empty = Empty

instance Show Square where
  show Black = "X"
  show White = "O"
  show Empty = "."

data Reversi = Reversi (Array Int Square) Square

letters = "abcdefgh"
size = length letters  -- must be even, or the starting board is weird and the game can't be played
sizeSq = size * size

data ReversiMove = Pass | MoveAt (Int, Int)
  deriving Eq

instance Show ReversiMove where
  show Pass = "pass"
  show (MoveAt (x, y)) = [letters !! x] ++ show (y + 1)

instance Read ReversiMove where
  readsPrec _ ('p' : 'a' : 's' : 's' : etc) = [(Pass, etc)]
  readsPrec _ (letter : digits) = case elemIndex (toLower letter) letters of
    Just x -> do (y, etc) <- readsPrec 0 digits
                 if 1 <= y && y <= size
                   then return (MoveAt (x, y - 1), etc)
                   else []
    _ -> []
  readsPrec _ _ = []

pointToIndex (x, y) = y * size + x
indexToPoint k = (k `mod` size, k `div` size)

instance Show Reversi where
  show (Reversi arr _) = concat
    [intercalate " " [show (arr ! pointToIndex (x, y))
                     | x <- [0 .. size - 1]] ++ "  " ++ show (y + 1) ++ "\n"
    | y <- reverse [0 .. size - 1]] ++ intersperse ' ' letters ++ "\n"

dirs = [(-1, -1), (-1, 0), (-1, 1),
        (0, -1),           (0, 1),
        (1, -1),  (1, 0),  (1, 1)]

captureIndexes arr me pt =
  let
    you = flipSquare me

    -- (ray dir point) computes the list of all indices of game squares in
    -- the ray starting at point and extending in the direction dir, not
    -- including point itself.

    ray' dir@(dx, dy) pt@(x, y) = if x < 0 || x >= size || y < 0 || y >= size
                               then []
                               else pointToIndex pt : ray' dir (x + dx, y + dy)
    ray dir@(dx, dy) (x, y)  = ray' dir (x + dx, y + dy)

    -- (rayFlips dir) returns the list of all indices of game squares in arr to
    -- the direction dir from pt, that have black pieces that will be flipped
    -- if we move at pt.
    rayFlips dir = case ray dir pt of
      xxs@(x : xs) | arr ! x == you ->
        case span (\ i -> arr ! i == you) xxs of
          (captured, further : _) | arr ! further == me -> captured
          _ -> []
      _ -> []

  in
    -- the list of all indices of black pieces that will be flipped
    concatMap rayFlips dirs

reversiApplyMove (Reversi arr turn) Pass = Reversi arr (flipSquare turn)
reversiApplyMove (Reversi arr turn) (MoveAt pt) =
  let places = pointToIndex pt : captureIndexes arr turn pt
      arr' = arr // [(j, turn) | j <- places]
  in Reversi arr' (flipSquare turn)

emptySquares arr = filter (\ x -> arr ! x == Empty) $ [0 .. sizeSq - 1]

canCapture (Reversi arr turn) k =
  not $ null $ captureIndexes arr turn $ indexToPoint k

capturingMoves g empties =
  map (MoveAt . indexToPoint) $ filter (canCapture g) $ empties

-- Super lame implementation, no elegance whatsoever
reversiMoves g@(Reversi arr turn) =
  let
    empties = emptySquares arr
    caps = capturingMoves g empties
  -- BUG: it is possible for both players to have no legal moves, in which case
  -- the game should end instead of both players passing forever.
  in if null empties
     then []
     else if not (null caps)
          then caps
          else if null (capturingMoves (Reversi arr (flipSquare turn)) empties)
               then []  -- neither player has any moves
               else [Pass]  -- other player has a move; this player must pass

count x lst = length $ filter (== x) lst

instance Game Reversi where
  type Move Reversi = ReversiMove
  start =
    let blank = replicate ((size * (size - 1) - 2) `div` 2) Empty
        mid = replicate (size - 2) Empty
        squareList = blank ++ [White, Black] ++ mid ++ [Black, White] ++ blank
    in Reversi (listArray (0, sizeSq - 1) squareList) Black

  moves = reversiMoves

  applyMove = reversiApplyMove

  scoreFinishedGame (Reversi arr you) =
    let squares = elems arr
        me = flipSquare you
    in fromIntegral (count me squares - count you squares)

positional arr you =
  let rubric = [
        15, -2,  3,  3,  3,  3, -2, 15,
        -2, -2, -1, -1, -1, -1, -2, -2,
         3, -1,  1,  1,  1,  1, -1,  3,
         3, -1,  1,  1,  1,  1, -1,  3,
         3, -1,  1,  1,  1,  1, -1,  3,
         3, -1,  1,  1,  1,  1, -1,  3,
        -2, -2, -1, -1, -1, -1, -2, -2,
        15, -2,  3,  3,  3,  3, -2, 15]
      mul (_, Empty)            = 0
      mul (value, x) | x == you = -value
      mul (value, _)            =  value
  in 0.005 * (sum $ map mul $ zip rubric $ elems arr)


heuristic (Reversi arr you) =
  let squares = elems arr
      me = flipSquare you
      yours = count you squares
      mine = count me squares
      delta = fromIntegral (mine - yours)
  in if yours + mine < 15 then -0.0025 * delta         -- opening: minimize
     else if yours + mine < 45 then positional arr you -- midgame: play positionally
     else 0.01 * delta                                 -- endgame: maximize

reversiSmartyPantsAI = bestMoveWithDepthLimit heuristic 4

main = playHumanVsComputer reversiSmartyPantsAI start

