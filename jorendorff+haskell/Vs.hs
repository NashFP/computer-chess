module Vs(playHumanVsComputer) where

import Minimax(applyMove, moves, scoreFinishedGame)
import System.IO(hFlush, stdout)

--- Command-line UI to play against a perfect computer opponent ---------------

--playHumanVsComputer :: (Game g, tons of other stuff) => g -> IO ()
playHumanVsComputer selectMove start = do
  putStrLn $ show start
  humansTurn start
  where
    --computersTurn :: g -> IO ()
    computersTurn board = do
      let move = selectMove board
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
      putStrLn $ "your legal moves are: " ++ show (moves board)
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
