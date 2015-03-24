{-# LANGUAGE TypeFamilies #-}

module Minimax(Game, Move, start, moves, applyMove, scoreFinishedGame, bestMove,
               bestMoveWithDepthLimit) where

import Data.List(maximumBy)

-- Hi! If this import doesn't work for you, you just need to `cabal install parallel`. :)
import Control.Parallel.Strategies(parMap, parPair, r0, rseq)
-- Or you can just delete it, and change the following line of code to:
--     pmap = map
--
-- pmap computes exactly the same thing as map, when it works. There are just two differences:
-- 1.  pmap can only produce lists of pairs.
-- 2.  pmap spreads out the computation across multiple CPU cores when you run with +RTS -N4.
pmap = parMap (parPair r0 rseq)

-- Takes a scoring function and returns the element of the list that has the
-- greatest score. This applies the scoring function once to each list element.
best :: Ord n => (x -> n) -> [x] -> x
best fn xs = fst $ maximumBy (\(a, as) (b, bs) -> compare as bs) $ pmap (\x -> (x, fn x)) xs


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
