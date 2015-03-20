{-# LANGUAGE TypeFamilies #-}

module Chess(Chessboard(..), Suite(..), ChessMove, ChessColor(..), chessAI) where

import Minimax
import Data.Bits(Bits, bit, shift, shiftR, shiftL, rotateL, (.&.), (.|.), complement, popCount)
import Data.Int
import Data.Word
import Data.List
import Vs

-- TODO castling, en passant

data ChessColor = Black | White
  deriving Eq

data Suite = Suite {
  pawns   :: Word64,
  knights :: Word64,
  bishops :: Word64,
  rooks   :: Word64,
  king    :: Word64}

data Chessboard = Chessboard {
  black    :: Suite,
  white    :: Suite,
  whoseTurn :: ChessColor }

wholeSuite s = pawns s .|. knights s .|. bishops s .|. rooks s .|. king s


--- Printing chessboards ------------------------------------------------------

data ChessPiece = King | Queen | Rook | Knight | Bishop | Pawn
  deriving Eq

whatsInSuiteAt s bit =
  if pawns s .&. bit /= 0 then Just Pawn
  else if knights s .&. bit /= 0 then Just Knight
  else if bishops s .&. bit /= 0 then Just (if rooks s .&. bit /= 0
                                            then Queen
                                            else Bishop)
  else if rooks s .&. bit /= 0 then Just Rook
  else if king s .&. bit /= 0 then Just King
  else Nothing

whatsAt g col row =
  let bit = shiftL (1 :: Word64) (8 * row + col)
  in case whatsInSuiteAt (black g) bit of
       Just piece -> Just (Black, piece)
       Nothing -> case whatsInSuiteAt (white g) bit of
         Just piece -> Just (White, piece)
         Nothing -> Nothing

instance Show Chessboard where
  show g =
    concat ["    " ++ row r ++ "   " ++ show (r + 1) ++ "\n" | r <- [7, 6 .. 0]]
    ++ "\n    a b c d e f g h\n"
    where
      row r = intersperse ' ' [toChar $ whatsAt g c r | c <- [0..7] ]
      toChar Nothing = '.'
      toChar (Just (White, King))   = 'K'
      toChar (Just (White, Queen))  = 'Q'
      toChar (Just (White, Rook))   = 'R'
      toChar (Just (White, Knight)) = 'N'
      toChar (Just (White, Bishop)) = 'B'
      toChar (Just (White, Pawn))   = 'P'
      toChar (Just (Black, King))   = 'k'
      toChar (Just (Black, Queen))  = 'q'
      toChar (Just (Black, Rook))   = 'r'
      toChar (Just (Black, Knight)) = 'n'
      toChar (Just (Black, Bishop)) = 'b'
      toChar (Just (Black, Pawn))   = 'p'


--- Flipping the table --------------------------------------------------------

flipColor White = Black
flipColor Black = White

flipSuite s = Suite {
  pawns    = flipBits $ pawns s,
  knights  = flipBits $ knights s,
  bishops  = flipBits $ bishops s,
  rooks    = flipBits $ rooks s,
  king     = flipBits $ king s}

-- Rotate a chessboard 180 degrees and swap the color of each piece.
flipBoard g = Chessboard {
  black     = flipSuite $ white g,
  white     = flipSuite $ black g,
  whoseTurn = flipColor $ whoseTurn g}

-- Reverse the order of the bits in a 64-bit integer.
--
-- Warren, Hacker's Delight, p. 101 gives a 32-bit version of this algorithm
-- and points out "These five assignment statements can be executed in any
-- order." Indeed so can ours, though for a completely different reason. :-)
flipBits :: Word64 -> Word64
flipBits u =
  let u2  = (shiftL (u   .&. 0x5555555555555555) 1) .|.
            (shiftR (u   .&. 0xaaaaaaaaaaaaaaaa) 1)
      u4  = (shiftL (u2  .&. 0x3333333333333333) 2) .|.
            (shiftR (u2  .&. 0xcccccccccccccccc) 2)
      u8  = (shiftL (u4  .&. 0x0f0f0f0f0f0f0f0f) 4) .|.
            (shiftR (u4  .&. 0xf0f0f0f0f0f0f0f0) 4)
      u16 = (shiftL (u8  .&. 0x00ff00ff00ff00ff) 8) .|.
            (shiftR (u8  .&. 0xff00ff00ff00ff00) 8)
      u32 = (shiftL (u16 .&. 0x0000ffff0000ffff) 16) .|.
            (shiftR (u16 .&. 0xffff0000ffff0000) 16)
  in rotateL u32 32


--- Reading and writing moves -------------------------------------------------

-- The optional piece at the end here is for pawn promotion.
data ChessMove = ChessMove Word64 Word64 (Maybe ChessPiece)
  deriving Eq

flipMove (ChessMove from to promote) = ChessMove (flipBits from) (flipBits to) promote

-- This only works for words with a single bit set.
log2OfBit x = countBit x 0xffffffff00000000 32 .|.
              countBit x 0xffff0000ffff0000 16 .|.
              countBit x 0xff00ff00ff00ff00  8 .|.
              countBit x 0xf0f0f0f0f0f0f0f0  4 .|.
              countBit x 0xcccccccccccccccc  2 .|.
              countBit x 0xaaaaaaaaaaaaaaaa  1
  where countBit x mask value = if x .&. mask == 0 then 0 else value

bitToChars b =
  let square = log2OfBit b
      row = square `div` 8
      col = square `mod` 8
  in ["abcdefgh" !! col, "12345678" !! row]

instance Show ChessMove where
  show (ChessMove from to promote) =
    bitToChars from ++ bitToChars to ++ case promote of
      Nothing     -> ""
      Just Queen  -> "q"
      Just Knight -> "n"
      Just Rook   -> "r"
      Just Bishop -> "b"

charsToBit :: Char -> Char -> Maybe Word64
charsToBit c r =
  let col = (fromEnum c - fromEnum 'a')
      row = (fromEnum r - fromEnum '1')
  in if 0 <= col && col < 8 && 0 <= row && row < 8
     then Just (bit (row * 8 + col))
     else Nothing

instance Read ChessMove where
  readsPrec _ (cf:rf:ct:rt:rest) = case (charsToBit cf rf, charsToBit ct rt) of
    (Just from, Just to) -> case rest of
      'q':rest' -> [(ChessMove from to (Just Queen), rest')]
      'n':rest' -> [(ChessMove from to (Just Knight), rest')]
      'r':rest' -> [(ChessMove from to (Just Rook), rest')]
      'b':rest' -> [(ChessMove from to (Just Bishop), rest')]
      _ -> [(ChessMove from to Nothing, rest)]
    _ -> []
  readsPrec _ _ = []


--- Rules of chess ------------------------------------------------------------

splitBit :: (Bits a, Num a) => a -> Maybe (a, a)
splitBit 0 = Nothing
splitBit x = let k = x .&. complement (x - 1)
             in Just (k, x .&. complement k)

-- Return a list of all the 1 bits set in the argument.
--     splitBits 0x89 == [0x01, 0x08, 0x80]
splitBits :: (Bits a, Num a) => a -> [a]
splitBits = unfoldr splitBit

-- rookDirs and friends are lists of pairs (shift, mask). 'shift' tells how
-- much to shift a bit to move the corresponding piece in this direction.
-- 'mask' tells whether a given piece is allowed to move this direction.
--
-- For example, suppose you have a rook at a1, which is bit 0.  That rook's
-- bit-pattern is 0x0000000000000001.  Bitwise AND that with each mask below to
-- see which directions the rook can move. It can't move toward white or toward
-- the queen's side, because it's already on the edge of the board. The other
-- directions are ok, so (shift 0x0000000000000001 1) and
-- (shift 0x0000000000000001 8) each produce new possible positions for the rook.
--
rookDirs :: [(Int, Word64)]
rookDirs = [
  (-8, 0xffffffffffffff00),  -- S, toward white
  (-1, 0xfefefefefefefefe),  -- W, toward the queen's side, I think
  ( 1, 0x7f7f7f7f7f7f7f7f),  -- E, toward the king's side
  ( 8, 0x00ffffffffffffff)]  -- N, toward black

bishopDirs :: [(Int, Word64)]
bishopDirs = [
  (-9, 0xfefefefefefefe00),  -- SW
  (-7, 0x7f7f7f7f7f7f7f00),  -- SE
  ( 7, 0x00fefefefefefefe),  -- NW
  ( 9, 0x007f7f7f7f7f7f7f)]  -- NE

kingDirs = rookDirs ++ bishopDirs

knightDirs :: [(Int, Word64)]
knightDirs = [
  (-17, 0xfefefefefefe0000), -- SSW
  (-15, 0x7f7f7f7f7f7f0000), -- SSE
  (-10, 0xfcfcfcfcfcfcfc00), -- WSW
  ( -6, 0x3f3f3f3f3f3f3f00), -- ESE
  (  6, 0x00fcfcfcfcfcfcfc), -- WNW
  ( 10, 0x003f3f3f3f3f3f3f), -- ENE
  ( 15, 0x0000fefefefefefe), -- NNW
  ( 17, 0x00007f7f7f7f7f7f)] -- NNE

whitePawnCaptureDirs :: [(Int, Word64)]
whitePawnCaptureDirs = [
  ( 7, 0x00fefefefefefefe),  -- NW
  ( 9, 0x007f7f7f7f7f7f7f)]  -- NE

blackPawnCaptureDirs :: [(Int, Word64)]
blackPawnCaptureDirs = [
  (-9, 0xfefefefefefefe00),  -- SW
  (-7, 0x7f7f7f7f7f7f7f00)]  -- SE

rank2 = 0x000000000000ff00  -- white pawns start here
rank7 = 0x00ff000000000000  -- black pawns start here

-- Return the list of all moves, without eliminating moves that leave
-- the current player's king in check. Moves where a piece would capture
-- an opposing king are also included (indeed we rely on this).
naiveMoves :: Chessboard -> [ChessMove]
naiveMoves g =
  let
    (friendly, enemy, pawnHomeRow, pawnPromoteRow, pawnShiftAmount, pawnCaptureDirs) =
      case whoseTurn g of
        White -> (white g, black g, rank2, rank7, 8, whitePawnCaptureDirs)
        Black -> (black g, white g, rank7, rank2, -8, blackPawnCaptureDirs)

    friendlyPieces = wholeSuite friendly
    enemyPieces = wholeSuite enemy
    allPieces = friendlyPieces .|. enemyPieces

    listSingleMovesByDir :: (Suite -> Word64) -> (Int, Word64) -> [ChessMove]
    listSingleMovesByDir field (shiftAmount, mask) =
      concatMap (trySingleMove shiftAmount) $ splitBits $ field friendly .&. mask

    -- Try a single move. We are already guaranteed that the piece will land on
    -- the board when we move it in this direction; but we still need to check
    -- that we are not trying to move on top of one of our own pieces. If this
    -- is a legal move, return a list containing just one element, this move:
    -- [ChessMove fromBit toBit]. Otherwise return the empty list.
    trySingleMove :: Int -> Word64 -> [ChessMove]
    trySingleMove shiftAmount b =
      let after :: Word64
          after = shift b shiftAmount
      in if (after .&. friendlyPieces) == 0
         then [ChessMove b after Nothing]
         else []

    listRayMovesByDir :: Word64 -> (Int, Word64) -> [ChessMove]
    listRayMovesByDir b (shiftAmount, mask) = [ChessMove b x Nothing | x <- stoppingPlaces b]
      where stoppingPlaces c = if c .&. mask == 0
                               then []
                               else let c' = shift c shiftAmount
                                    in if c' .&. friendlyPieces /= 0
                                       then []
                                       else if c' .&. enemyPieces /= 0
                                            then [c']
                                            else c' : stoppingPlaces c'

    listRayMoves :: [(Int, Word64)] -> Word64 -> [ChessMove]
    listRayMoves dirs b = concatMap (listRayMovesByDir b) dirs

    listPawnMoves b =
      let
        dest1 = shift b pawnShiftAmount
        move1 = ChessMove b dest1 Nothing
        dest2 = shift dest1 pawnShiftAmount
        move2 = ChessMove b dest2 Nothing
        pawnForwardMoves =
          if allPieces .&. dest1 == 0
          then if b .&. pawnHomeRow /= 0  -- pawn is in home row
                  && allPieces .&. dest2 == 0
               then [move1, move2]
               else [move1]
          else []
        listPawnCaptures (shiftAmount, mask) =
          let dest = shift b shiftAmount
          in if b .&. mask /= 0  &&  dest .&. enemyPieces /= 0
             then [ChessMove b dest Nothing]
             else []
        pawnCaptures = concatMap listPawnCaptures pawnCaptureDirs
        movesWithoutPromotion = pawnForwardMoves ++ pawnCaptures
        promote (ChessMove orig dest _) =
          [ChessMove orig dest (Just p) | p <- [Queen, Knight, Rook, Bishop]]
      in if b .&. pawnPromoteRow /= 0  -- pawn is in seventh rank
         then concatMap promote movesWithoutPromotion
         else movesWithoutPromotion

    my x = x friendly
    pawnMoves = concatMap listPawnMoves $ splitBits $ my pawns
    knightMoves = concatMap (listSingleMovesByDir knights) knightDirs
    bishopMoves = concatMap (listRayMoves bishopDirs) $ splitBits $ my bishops
    rookMoves = concatMap (listRayMoves rookDirs) $ splitBits $ my rooks
    kingMoves = concatMap (listSingleMovesByDir king) kingDirs
  in if my king == 0
     then []
     else pawnMoves ++ knightMoves ++ bishopMoves ++ rookMoves ++ kingMoves

-- Given the board 'g' and a bit 'square', return true if any of 'color's
-- pieces are attacking that square.
squareIsThreatenedBy g square color =
  let hypothetical = g {whoseTurn = color}
  in any (\(ChessMove _ to _ ) -> to == square) $ naiveMoves hypothetical

legalMoves g = filter (not . leavesSelfInCheck) (naiveMoves g)
  where
    -- Suppose it's white to play. Then m is a move by white, and g' is the board
    -- after that move. In g' it is black's turn. We want to know if white's king
    -- is threatened by black's pieces.
    leavesSelfInCheck m =
      let me = whoseTurn g
          you = flipColor me
          g' = applyMove g m
          myKing = king $ case me of
            White -> white g'
            Black -> black g'
      in squareIsThreatenedBy g' myKing you

applyWhiteMove g (ChessMove fromBit toBit promote) =
  let
    applyMoveToBitBoard bits =
      if bits .&. fromBit == 0
      then bits
      else (bits .&. complement fromBit) .|. toBit

    gWhite = white g
    gBlack = black g

    g' = Chessboard {
       black = Suite {
         pawns   = pawns gBlack   .&. complement toBit,
         knights = knights gBlack .&. complement toBit,
         bishops = bishops gBlack .&. complement toBit,
         rooks   = rooks gBlack   .&. complement toBit,
         king    = king gBlack    .&. complement toBit},
       white = Suite {
         pawns   = applyMoveToBitBoard $ pawns gWhite,
         knights = applyMoveToBitBoard $ knights gWhite,
         bishops = applyMoveToBitBoard $ bishops gWhite,
         rooks   = applyMoveToBitBoard $ rooks gWhite,
         king    = applyMoveToBitBoard $ king gWhite},
       whoseTurn = Black}
  in case promote of
    Nothing -> g'
    Just piece ->
      let w' = white g'
          w'' = w' {pawns = pawns w' .&. complement toBit}
          w''' = case piece of
            Queen  -> w'' {bishops = bishops w'' .|. toBit,
                           rooks   = rooks   w'' .|. toBit}
            Knight -> w'' {knights = knights w'' .|. toBit}
            Bishop -> w'' {bishops = bishops w'' .|. toBit}
            Rook   -> w'' {rooks   = rooks   w'' .|. toBit}
      in g' {white = w'''}

instance Game Chessboard where
  type Move Chessboard = ChessMove

  start = Chessboard {
    black = Suite {
      pawns   = 0x00ff000000000000,
      knights = 0x4200000000000000,
      bishops = 0x2c00000000000000,
      rooks   = 0x8900000000000000,
      king    = 0x1000000000000000},
    white = Suite {
      pawns   = 0x000000000000ff00,
      knights = 0x0000000000000042,
      bishops = 0x000000000000002c,
      rooks   = 0x0000000000000089,
      king    = 0x0000000000000010},
    whoseTurn = White}

  moves = legalMoves

  applyMove g move = case whoseTurn g of
    White -> applyWhiteMove g move
    Black -> flipBoard $ applyWhiteMove (flipBoard g) (flipMove move)

  scoreFinishedGame g =
    let side = case whoseTurn g of
          White -> white g
          Black -> black g
    in if squareIsThreatenedBy g (king side) (flipColor $ whoseTurn g) then 1 else 0

-- The first heuristic I attempted was this amazingly bad one:
heuristic0 g = 0

-- Then I tried this almost-as-bad heuristic: just count pieces :)
heuristic1 g = 0.05 * fromIntegral (popCount (wholeSuite $ black g) - popCount (wholeSuite $ white g))

-- Now we're at the point where we give the pieces values. The scores here are
-- from Wikipedia, which lists a dozen or more scoring systems to choose
-- from. This one is credited to Hans Berliner.
--     Since we have this bizarre thing where a queen is represented as a
-- bishop and a rook, the code below counts the queen first as a bishop and
-- then as a rook, for 3.33 + 5.1 = 8.43 points. But in Berliner's system a
-- queen is worth 8.80 points, so we add another line of code to add .37 points
-- if the queen is still around.
heuristic g =
  let diff field = fromIntegral $ popCount (field (black g)) - popCount (field (white g))
      totalDiff = diff pawns
                  + 3.2 * diff knights
                  + 3.33 * diff bishops
                  + 5.1 * diff rooks
                  + 0.37 * diff (\s -> bishops s .&. rooks s)
      signedDiff = case whoseTurn g of
        White -> totalDiff
        Black -> -totalDiff
  in 0.001 * signedDiff

chessAI = bestMoveWithDepthLimit heuristic 3
