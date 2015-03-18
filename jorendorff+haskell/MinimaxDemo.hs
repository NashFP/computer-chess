{-# LANGUAGE TypeFamilies #-}

import Minimax
import Vs
import Data.Bits

--- Some example games --------------------------------------------------------


--- Pennies

data Pennies = Pennies Int
  deriving Show

instance Game Pennies where
  type Move Pennies = Int
  start = Pennies 11
  moves (Pennies n) = filter (<= n) [1, 2, 3]
  applyMove (Pennies n) k = Pennies (n - k)
  scoreFinishedGame (Pennies 0) = 1


--- Fifteen

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


--- FifteenBits

data FifteenBits = FifteenBits Int Int

numberToBit n = shiftL 1 n

bitsToList x = filter (\n -> (numberToBit n .&. x) /= 0) [1..9]

instance Show FifteenBits where
  show (FifteenBits xs os) =
    show (Fifteen (bitsToList xs) (bitsToList os))

wins = [numberToBit i .|. numberToBit j .|. numberToBit k
        | i <- [1..7], j <- [i+1..8], k <- [j+1..9], i + j + k == 15]

bitsHasWin bits = any (\win -> (bits .&. win) == win) wins

instance Game FifteenBits where
  type Move FifteenBits = Int
  start = FifteenBits 0 0
  moves g@(FifteenBits xs os) =
    if bitsHasWin os then []
    else filter (\x -> (numberToBit x .&. (xs .|. os)) == 0) [1..9]
  applyMove (FifteenBits xs os) x = FifteenBits os (numberToBit x .|. xs)
  scoreFinishedGame (FifteenBits xs os) = if bitsHasWin os then 1 else 0


--- Let's play!

penniesSmartyPantsAI :: Pennies -> Int
penniesSmartyPantsAI =
  bestMoveWithDepthLimit (\(Pennies n) -> if n `mod` 4 == 0 then 1.0 else -1.0) 2

main = playHumanVsComputer penniesSmartyPantsAI (Pennies 30)
