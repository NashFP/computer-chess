{-# LANGUAGE TypeFamilies #-}

module Minimax(Game, Move, start, moves, applyMove, scoreFinishedGame, bestMove,
               bestMoveWithDepthLimit, bestMoveWithTimeLimit) where

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


--- How to play a game when you are in a hurry --------------------------------

bestMoveWithDepthLimit estimator limit g =
  best (scoreMoveWithDepthLimit estimator limit g) (moves g)

scoreMoveWithDepthLimit estimator limit g m =
  scoreGameWithDepthLimit estimator limit (applyMove g m)

scoreGameWithDepthLimit estimator limit g = case moves g of
  [] -> scoreFinishedGame g
  ms -> if limit == 0
        then estimator g
        else -maximum (map (scoreMoveWithDepthLimit estimator (limit - 1) g) ms)


--- Another approach ----------------------------------------------------------

bestMoveWithTimeLimit estimator limit g =
  let moveList = moves g
      limitPerMove = limit `div` length moveList
  in best (scoreMoveWithTimeLimit estimator limitPerMove 1 g) (moves g)

scoreMoveWithTimeLimit estimator limit debugDepth g m =
  let score = scoreGameWithTimeLimit estimator limit debugDepth (applyMove g m)
  in trace (take (debugDepth * 4) (repeat ' ') ++ show m ++ " - " ++ show score) score

scoreGameWithTimeLimit estimator limit debugDepth g = case moves g of
  [] -> scoreFinishedGame g
  moveList -> let limitPerMove = limit `div` length moveList
              in if limit == 0
                 then estimator g
                 else -maximum (map (scoreMoveWithTimeLimit estimator limitPerMove (debugDepth + 1) g) moveList)
