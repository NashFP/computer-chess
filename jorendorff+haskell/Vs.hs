module Vs(playHumanVsComputer) where

import Minimax(applyMove, moves, scoreFinishedGame)
import System.IO(hFlush, stdout)
import Data.Char(isSpace)

--- Command-line UI to play against a computer opponent -----------------------

inputOneOf :: (Show a, Read a, Eq a) => [a] -> String -> IO (Maybe a)
inputOneOf options prompt = do
  putStr prompt
  hFlush stdout
  line <- getLine
  if line == "?"
    then retry ("options: " ++ show options)
    else if line == "q"
         then return Nothing
         else do
           case reads line of
             (value, rest) : _ ->
               if all isSpace rest
               then if elem value options
                    then return $Just value
                    else retry $ show value ++ " is not an option (enter ? to show all options)"
               else retry "i didn't understand all of that"
             _ -> retry "i didn't understand that"
  where retry msg = putStrLn msg >> inputOneOf options prompt


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
      maybeMove <- inputOneOf (moves board) "your turn> "
      case maybeMove of
        Nothing -> return ()  -- user typed "q" to quit
        Just move -> do
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
