{-# LANGUAGE TypeFamilies #-}

import Minimax
import Vs

data Fifteen = Fifteen [Int] [Int]
  deriving Show

hasWin xs = any (== 15) [i + j + k | i <- xs, j <- xs, i < j, k <- xs, j < k]

instance Game Fifteen where
  type Move Fifteen = Int
  start = Fifteen [] []
  moves g@(Fifteen xs os) =
    if hasWin os then []
    else filter (\x -> not (elem x xs || elem x os)) [1..9]
  applyMove (Fifteen xs os) x = Fifteen os (x:xs)
  scoreFinishedGame (Fifteen xs os) = if hasWin os then 1 else 0

main = playHumanVsComputer (bestMove :: Fifteen -> Move Fifteen) start
