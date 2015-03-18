module Main where

import Board
import AI
import Control.Monad.State
import Data.Maybe
import System.Exit
import System.IO

main :: IO ()
main = runStateT commandLoop startingTree >> return ()
 
commandLoop :: StateT MoveTree IO ()
commandLoop = forever $ do thisLine <- lift getLine
                           let command = head $ words thisLine
                           let arguments = drop ((length command) + 1) thisLine
                           io $ appendFile "xboard-input.txt" $ "--> " ++ thisLine ++ "\n"                           
                           if command `elem` ignoredCommands
                           then return ()
                           else case command of
                                  "new"      -> put startingTree
                                  "protover" -> io $ respond $ "feature variants=\"normal\" usermove=1 draw=0 analyze=0 colors=0 setboard=1 sigint=0 done=1"
                                  "quit"     -> io $ exitWith ExitSuccess
                                  "setboard" -> do io $ appendFile "xboard-input.txt" arguments
                                                   put $ populateMoveTreeToDepth defaultDepth $ MoveTree (fenToBoard arguments) NoMove [] []
                                                   treeNow <- get
                                                   let boardNow = boardOf treeNow
                                                   io $ appendFile "xboard-input.txt" $ show boardNow
                                                   io $ appendFile "xboard-input.txt" $ (scoreBreakdown boardNow) ++ "\n"
                                                   io $ appendFile "xboard-input.txt" $ (boardToFen boardNow) ++ "\n"
                                  "usermove" -> do result <- tryMove arguments
                                                   treeNow <- get
                                                   if result == ""
                                                   then do let optimal = optimalMove treeNow
                                                           put $ selectMove treeNow optimal
                                                           io $ respond $ "move " ++ (show optimal)
                                                   else io $ respond $ result
                                                   treeNow <- get
                                                   let boardNow = boardOf treeNow
                                                   io $ appendFile "xboard-input.txt" $ show boardNow
                                                   io $ appendFile "xboard-input.txt" $ (scoreBreakdown boardNow) ++ "\n"
                                                   io $ appendFile "xboard-input.txt" $ (boardToFen boardNow) ++ "\n"
                                  otherwise  -> io $ respond $ "Error: (unknown command): " ++ thisLine

respond :: String -> IO ()
respond str = do putStrLn str
                 hFlush stdout
                 appendFile "xboard-input.txt" $ (concat $ map ("<-- " ++) (lines str)) ++ "\n"

ignoredCommands = ["xboard", "accepted", "rejected", "variant", "random", "force", "go", "white", "black", "level", "st", "sd", "nps", "time", "otim", "?", "ping", "draw", "result", "edit", "hint", "bk", "undo", "remove", "hard", "easy", "post", "nopost", "analyze", "name", "rating", "computer", "option"]
 
io :: IO a -> StateT MoveTree IO a
io = liftIO

tryMove :: String -> StateT MoveTree IO String
tryMove str = do treeNow <- get
                 let oldBoard = boardOf treeNow
                 if (not $ isMove str)
                 then return $ "Error (unrecognized move): " ++ str
                 else do let result = makeMove oldBoard (read str)
                         if (result == Nothing)
                         then return $ "Illegal move: " ++ str
                         else do put $ selectMove treeNow (read str)
                                 treeNow <- get
                                 let newBoard = boardOf treeNow
                                 if (isInCheckmate newBoard Black)
                                 then return "0-1 {Black mates}"
                                 else if (isInCheckmate newBoard White)
                                      then return "1-0 {White mates}"
                                      else if ((isInStalemate newBoard Black) || (isInStalemate newBoard White))
                                           then return "1/2-1/2 {Stalemate}"
                                           else return ""

isMove :: String -> Bool
isMove (f1:r1:f2:r2:[])   = isFile f1 && isRank r1 && isFile f2 && isRank r2
isMove (f1:r1:f2:r2:p:[]) = isFile f1 && isRank r1 && isFile f2 && isRank r2 && (p `elem` "pnbrqk")
isMove _ = False

promotionPiece :: String -> Char
promotionPiece (a:b:c:d:e:_) = e
promotionPiece _ = 'q'

isFile :: Char -> Bool
isFile ch = ch `elem` "abcdefgh"

isRank :: Char -> Bool
isRank ch = ch `elem` "12345678"
