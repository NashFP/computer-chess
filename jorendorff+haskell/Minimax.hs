{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module Minimax(Game, Move, start, moves, applyMove, scoreFinishedGame, bestMove,
               bestMoveWithDepthLimit,
               bestMoveWithDepthLimitOriginal,
               bestMoveWithDepthLimit') where

import Data.List(maximumBy, foldl')

-- Hi! If this import doesn't work for you, you just need to `cabal install parallel`. :)
import Control.Parallel.Strategies(parMap, parTuple2, r0, rseq)
-- Or you can just delete it, and change the following line of code to:
--     pmap = map
--
-- pmap computes exactly the same thing as map, when it works. There are just two differences:
-- 1.  pmap can only produce lists of pairs.
-- 2.  pmap spreads out the computation across multiple CPU cores when you run with +RTS -N4.
pmap = parMap (parTuple2 r0 rseq)

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

bestMoveWithDepthLimitOriginal estimator moveLimit g =
  let halfMoveLimit = moveLimit * 2 - 1
  in best (scoreMoveWithDepthLimit estimator halfMoveLimit g) (moves g)

scoreMoveWithDepthLimit estimator limit g m =
  scoreGameWithDepthLimit estimator limit (applyMove g m)

scoreGameWithDepthLimit estimator limit g = case moves g of
  [] -> scoreFinishedGame g
  ms -> if limit == 0
        then estimator g
        else -- This 0.999 makes near-term game-winning moves more attractive than distant ones,
             -- so the AI doesn't drag a game out unnecessarily. (By the same token, this makes
             -- the AI drag out losing games, but it's the winner's responsibility to end it.)
             -0.999 * maximum (map (scoreMoveWithDepthLimit estimator (limit - 1) g) ms)


--- Or equivalently -----------------------------------------------------------

infinity = 1/0
negativeInfinity = -infinity

bestMoveWithDepthLimit' :: forall g . Game g => (g -> Float) -> Int -> (g -> Move g)
bestMoveWithDepthLimit' estimator moveLimit g =
  best (scoreMyMove (moveLimit - 1) g) (moves g)
  where
    scoreMyMove :: Int -> g -> Move g -> Float
    scoreMyMove limit g0 m =
      let g = applyMove g0 m
      in case moves g of
        [] -> scoreFinishedGame g
        replyList -> 0.999 * minimum (map (scoreYourReply limit g) replyList)

    scoreYourReply :: Int -> g -> Move g -> Float
    scoreYourReply limit g0 m =
      let g = applyMove g0 m
      in case moves g of
           [] -> -scoreFinishedGame g
           -- This 0.999 makes near-term game-winning moves more attractive than distant ones,
           -- so the AI doesn't drag a game out unnecessarily. (By the same token, this makes
           -- the AI drag out losing games, but it's the winner's responsibility to end it.)
           moveList ->
             if limit <= 0
             then -estimator g
             else 0.999 * myMaxScore limit g moveList

    myMaxScore :: Int -> g -> [Move g] -> Float
    myMaxScore limit g moveList =
      foldl' (considerMyMove (limit - 1) g) negativeInfinity moveList

    considerMyMove :: Int -> g -> Float -> Move g -> Float
    considerMyMove limit g0 previousBest m =
      let g = applyMove g0 m
      in case moves g of
        [] -> max previousBest $ scoreFinishedGame g
        replyList -> myScoreWithYourBestReply limit g previousBest replyList

    myScoreWithYourBestReply :: Int -> g -> Float -> [Move g] -> Float
    myScoreWithYourBestReply limit g previousBest replyList =
      foldr (considerYourReply limit g previousBest) infinity replyList

    considerYourReply :: Int -> g -> Float -> Move g -> Float -> Float
    considerYourReply limit g0 previousBest m currentWorst =
      let
        g = applyMove g0 m
        result = 0.999 * case moves g of
          [] -> -scoreFinishedGame g
          moveList ->
            if limit <= 0
            then -estimator g
            else 0.999 * myMaxScore limit g moveList
      in if result < previousBest
         then previousBest  -- cut! ignore currentWorst; whole tail of foldr isn't computed
         else min currentWorst result

bestMoveWithDepthLimit estimator moveLimit g =
  let a = bestMoveWithDepthLimitOriginal estimator moveLimit g
      b = bestMoveWithDepthLimit' estimator moveLimit g
  in if a == b
     then a
     else error ("bug: original algorithm moved " ++ show a ++ "; new algorithm moved " ++ show b)
