{-# LANGUAGE TypeFamilies #-}

module Minimax(Game, Move, start, moves, applyMove, scoreFinishedGame, bestMove) where

import Data.List(maximumBy)

-- Takes a scoring function and returns the element of the list that has the
-- greatest score. This applies the scoring function once to each list element.
best :: Ord n => (x -> n) -> [x] -> x
best fn xs = fst $ maximumBy (\(a, as) (b, bs) -> compare as bs) $ map (\x -> (x, fn x)) xs



--- The essence of a game -----------------------------------------------------

class Game g where
  type Move g
  start :: g
  moves :: g -> [Move g]
  applyMove :: g -> Move g -> g
  scoreFinishedGame :: g -> Float  -- returns >0 if last move won the game.


--- How to play a game, if you're a computer ----------------------------------

bestMove g = best (scoreMove g) (moves g)

scoreMove g m = scoreGame (applyMove g m)

scoreGame g = case moves g of
  [] -> scoreFinishedGame g
  ms -> -maximum (map (scoreMove g) ms)



