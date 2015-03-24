module ChessAI(chessAI) where

import Minimax(bestMoveWithDepthLimit)
import Data.Bits(popCount, complement, (.&.))
import Chess(Chessboard(..), ChessColor(..), Suite(..), black, white, wholeSuite)

-- The first heuristic I attempted was this amazingly bad one:
heuristic0 g = 0

-- Then I tried this almost-as-bad heuristic: just count pieces :)
heuristic1 g = 0.05 * fromIntegral (popCount (wholeSuite $ black g) - popCount (wholeSuite $ white g))

-- Now we're at the point where we give the pieces values.
heuristic g =
  let -- The scores here are from Wikipedia, which lists a dozen or more
      -- scoring systems to choose from. This one is credited to Hans Berliner.
      --
      -- This code is complicated slightly by our trick of representing a queen
      -- as a bishop and a rook. For speed, we do not bother eliminating queens
      -- when counting the number of bishops/rooks, but rather count queens as
      -- both bishops and rooks, and then add a bit *more*...
      total side =   100 * popCount (pawns side)
                   + 320 * popCount (knights side)
                   + 333 * popCount (bishops side)
                   + 510 * popCount (rooks side)
                   +  37 * popCount (bishops side .&. rooks side)
      diff = 0.00001 * fromIntegral (total (black g) - total (white g))
  in case whoseTurn g of
       White -> diff
       Black -> -diff

chessAI = bestMoveWithDepthLimit heuristic 4
