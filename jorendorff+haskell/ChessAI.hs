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
  let count field side = fromIntegral $ popCount $ field side

      -- This code is complicated slightly by our trick of representing a queen
      -- as a bishop and a rook.
      justBishops side = bishops side .&. complement (rooks side)
      justRooks side = rooks side .&. complement (bishops side)
      queens side = bishops side .&. rooks side

      -- The scores here are from Wikipedia, which lists a dozen or more
      -- scoring systems to choose from. This one is credited to Hans Berliner.
      total side = count pawns side
                   + 3.2 * count knights side
                   + 3.33 * count justBishops side
                   + 5.1 * count justRooks side
                   + 8.80 * count queens side
      diff = 0.001 * (total (black g) - total (white g))
  in case whoseTurn g of
       White -> diff
       Black -> -diff

chessAI = bestMoveWithDepthLimit heuristic 3
