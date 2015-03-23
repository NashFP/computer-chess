{-# LANGUAGE TypeFamilies #-}

import Minimax
import Vs

data Pennies = Pennies Int
  deriving Show

instance Game Pennies where
  type Move Pennies = Int
  start = Pennies 11
  moves (Pennies n) = filter (<= n) [1, 2, 3]
  applyMove (Pennies n) k = Pennies (n - k)
  scoreFinishedGame (Pennies 0) = 1

main = playHumanVsComputer bestMove (Pennies 14)

-- penniesSmartyPantsAI :: Pennies -> Move Pennies
-- penniesSmartyPantsAI =
--   bestMoveWithDepthLimit (\(Pennies n) -> if n `mod` 4 == 0 then 1.0 else -1.0) 2
