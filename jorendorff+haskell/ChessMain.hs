import System.Environment(getArgs)
import Minimax(start)
import ChessAI(chessAI)
import XBoard(playXBoard)
import Vs(playHumanVsComputer)

main = do
  args <- getArgs
  case args of
       ["--xboard"] -> playXBoard chessAI
       [] -> playHumanVsComputer chessAI start
