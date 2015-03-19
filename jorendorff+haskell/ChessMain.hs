import Control.Monad.State(runStateT)
import System.Environment(getArgs)
import Minimax(start)
import Chess(chessAI)
import XBoard(playXBoard)
import Vs(playHumanVsComputer)

main = do
  args <- getArgs
  case args of
       ["--xboard"] -> playXBoard chessAI
       [] -> playHumanVsComputer chessAI start
