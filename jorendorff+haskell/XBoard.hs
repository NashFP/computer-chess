module XBoard(playXBoard, fenToBoard) where

import Control.Monad(forever)
import System.IO(hFlush, stdout)
import Data.IORef(newIORef, readIORef, writeIORef, modifyIORef)
import System.Exit(exitWith, ExitCode(ExitSuccess))
import Data.Char(isSpace, ord)
import Data.Bits(bit, (.|.), shiftL)
import Minimax(start, moves, applyMove, scoreFinishedGame)
import Chess(Chessboard(..), Suite(..), ChessMove, ChessColor(White, Black))
import ChessAI(chessAI)

data MoveResult = MoveError String | GameOver String | Continue Chessboard

tryMove :: Chessboard -> String -> MoveResult
tryMove board str = case reads str of
  [] -> MoveError ("unrecognized move: " ++ str)
  (move, rest) : _ ->
    if not (all isSpace rest)
    then MoveError("unrecognized move: " ++ str)
    else if not (elem move (moves board))
         then MoveError ("illegal move: " ++ show move)
         else let result = applyMove board move
              in case moves result of
                   [] -> GameOver (case scoreFinishedGame result of
                                      0 -> "1/2-1/2 {stalemate}"
                                      _ -> if whoseTurn result == Black
                                           then "1-0 {White mates}"
                                           else "0-1 {Black mates}")
                   _ -> Continue result

ignoredCommands = [
  "xboard", "accepted", "rejected", "variant", "random", "white", "black", "level",
  "st", "sd", "nps", "time", "otim", "?", "ping", "draw", "result", "edit", "hint", "bk", "undo",
  "remove", "hard", "easy", "post", "nopost", "analyze", "name", "rating", "computer", "option"]

respond :: String -> IO ()
respond str = do
  putStrLn str
  hFlush stdout
  appendFile "xboard-input.txt" $ (concat $ map ("<-- " ++) (lines str)) ++ "\n"

say str = appendFile "xboard-input.txt" str

fileToInt :: Char -> Int
fileToInt file = (ord file) - (ord 'a')

fenToBoard :: String -> Chessboard
fenToBoard str = Chessboard {
  white = Suite {
    pawns   = bitBoard "P",
    knights = bitBoard "N",
    bishops = bitBoard "BQ",
    rooks   = bitBoard "RQ",
    king    = bitBoard "K",
    castleK = 'K' `elem` castlingString,
    castleQ = 'Q' `elem` castlingString},
  black = Suite {
    pawns   = bitBoard "p",
    knights = bitBoard "n",
    bishops = bitBoard "bq",
    rooks   = bitBoard "rq",
    king    = bitBoard "k",
    castleK = 'k' `elem` castlingString,
    castleQ = 'q' `elem` castlingString},
  whoseTurn = whoseTurn,
  enPassant = enPassant}
  where
    piecesStr : whoseTurnStr : castlingString : enPassantString : _ = words str

    bitBoard chars = foldr (.|.) 0
                     $ map (\(c, b) -> if c `elem` chars then b else 0)
                     $ zip array (map bit [0..63])

    array = concat $ reverse $ splitLength 8
            $ filter (/= '/') $ expandSpaces piecesStr

    expandSpaces :: String -> String
    expandSpaces (x:xs)
        | x `elem` ['1'..'8'] = (replicate (read [x]) ' ') ++ (expandSpaces xs)
        | otherwise = x:(expandSpaces xs)
    expandSpaces "" = ""

    splitLength n xs@(x:_) = take n xs : (splitLength n $ drop n xs)
    splitLength _ [] = []
    whoseTurn = case whoseTurnStr of
                  "w" -> White
                  "b" -> Black

    enPassant = case enPassantString of
      "-" -> 0
      a : _ -> shiftL pawnRow $ fileToInt a
      where
        pawnRow = case whoseTurn of
          White -> 0x0000010000000000
          Black -> 0x0000000000010000

get = readIORef
set = writeIORef

playXBoard :: (Chessboard -> ChessMove) -> IO ()
playXBoard ai = do
  boardRef <- newIORef start
  forceModeRef <- newIORef True

  let go = do board <- get boardRef
              let move = ai board
              respond $ "move " ++ (show move)
              set boardRef $ applyMove board move

  forever $ do
    thisLine <- getLine
    let command = head $ words thisLine
    let arguments = drop ((length command) + 1) thisLine
    say $ "--> " ++ thisLine ++ "\n"
    if command `elem` ignoredCommands
    then return ()
    else case command of
      "new"      -> do set boardRef start
                       set forceModeRef False
      "protover" -> respond $ "feature variants=\"normal\" usermove=1 draw=0 analyze=0 colors=0 "
                              ++ "setboard=1 sigint=0 done=1"
      "quit"     -> exitWith ExitSuccess
      "force"    -> set forceModeRef True
      "setboard" -> do say arguments
                       let boardNow = fenToBoard arguments
                       set boardRef boardNow
                       say $ show boardNow
      "go"       -> do set forceModeRef False
                       go
                       boardNow <- get boardRef
                       say $ show $ boardNow
      "usermove" -> do board <- get boardRef
                       case tryMove board arguments of
                         MoveError msg -> respond msg
                         GameOver msg -> respond msg
                         Continue board' -> do
                           set boardRef board'
                           forceMode <- get forceModeRef
                           if forceMode then return () else go
                       board <- get boardRef
                       say $ show board
      _          -> respond $ "Error: (unknown command): " ++ thisLine
