{-# LANGUAGE TypeFamilies #-}

import Data.List(maximumBy)
import Data.Bits
import System.IO(stdout, hFlush)

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



--- Command-line UI to play against a perfect computer opponent ---------------

--playHumanVsComputer :: (Game g, tons of other stuff) => g -> IO ()
playHumanVsComputer start = do
  putStrLn $ show start
  humansTurn start
  where
    --computersTurn :: g -> IO ()
    computersTurn board = do
      let move = bestMove board
      putStrLn $ "my move: " ++ show move
      let newBoard = applyMove board move
      putStrLn $ show newBoard
      case moves newBoard of
        [] -> do  --human has no moves; game is over
          putStrLn "game over"
          putStrLn $ case scoreFinishedGame newBoard of
            0 -> "it's a tie"
            x | x > 0 -> "i win"
            _ -> "you win"
        _ ->
          humansTurn newBoard

    --humansTurn :: g -> IO ()
    humansTurn board = do
      putStr "your turn> "
      hFlush stdout
      line <- getLine
      if line == "q" then return () else do
        let move = read line
        let newBoard = applyMove board move
        putStrLn $ show newBoard
        case moves newBoard of
          [] -> do  -- computer has no moves; game is over
            putStrLn "game over"
            putStrLn $ case scoreFinishedGame newBoard of
              0 -> "it's a tie"
              x | x > 0 -> "you win"
              _ -> "i win"
          _ ->
            computersTurn newBoard

main = playHumanVsComputer (start :: Pennies)
