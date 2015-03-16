{-# LANGUAGE TypeFamilies #-}

import Minimax
import Data.Bits(Bits, bit, shift, shiftR, shiftL, rotateL, (.&.), (.|.), complement, popCount)
import Data.Int
import Data.Word
import Data.List
import Vs

-- TODO castling, en passant

data ChessColor = Black | White
  deriving Eq

data Chessboard = Chessboard {
  bPawns   :: Word64,
  bKnights :: Word64,
  bBishops :: Word64,
  bRooks   :: Word64,
  bKing    :: Word64,
  wPawns   :: Word64,
  wKnights :: Word64,
  wBishops :: Word64,
  wRooks   :: Word64,
  wKing    :: Word64,
  whoseTurn :: ChessColor }

allWhitePieces g = wPawns g .|. wKnights g .|. wBishops g .|. wRooks g .|. wKing g
allBlackPieces g = bPawns g .|. bKnights g .|. bBishops g .|. bRooks g .|. bKing g


--- Printing chessboards ------------------------------------------------------

data ChessPiece = King | Queen | Rook | Knight | Bishop | Pawn
  deriving Eq

whatsAt g col row =
  let b = shiftL (1 :: Word64) (8 * row + col)
  in if (bPawns g .&. b) /= 0 then Just (Black, Pawn)
     else if (bKnights g .&. b) /= 0 then Just (Black, Knight)
     else if (bBishops g .&. b) /= 0 then Just (Black, if (bRooks g .&. b) /= 0
                                                         then Queen
                                                         else Bishop)
     else if (bRooks   g .&. b) /= 0 then Just (Black, Rook)
     else if (bKing    g .&. b) /= 0 then Just (Black, King)
     else if (wPawns   g .&. b) /= 0 then Just (White, Pawn)
     else if (wKnights g .&. b) /= 0 then Just (White, Knight)
     else if (wBishops g .&. b) /= 0 then Just (White, if (wRooks g .&. b) /= 0
                                                         then Queen
                                                         else Bishop)
     else if (wRooks   g .&. b) /= 0 then Just (White, Rook)
     else if (wKing    g .&. b) /= 0 then Just (White, King)
     else Nothing

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

-- Rotate a chessboard 180 degrees and swap the color of each piece.
flipBoard g = Chessboard {
  bPawns   = flipBits $ wPawns g,
  bKnights = flipBits $ wKnights g,
  bBishops = flipBits $ wBishops g,
  bRooks   = flipBits $ wRooks g,
  bKing    = flipBits $ wKing g,

  wPawns   = flipBits $ bPawns g,
  wKnights = flipBits $ bKnights g,
  wBishops = flipBits $ bBishops g,
  wRooks   = flipBits $ bRooks g,
  wKing    = flipBits $ bKing g,

  whoseTurn = flipColor $ whoseTurn g}

-- Reverse the order of the bits in a 64-bit integer.
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
  (-9, 0xfefefefefefefe00),  -- SW, I think
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

whiteMoves g =
  let
    whitePieces :: Word64
    whitePieces = allWhitePieces g

    blackPieces :: Word64
    blackPieces = allBlackPieces g

    allPieces = whitePieces .|. blackPieces

    listSingleMovesByDir :: (Chessboard -> Word64) -> (Int, Word64) -> [ChessMove]
    listSingleMovesByDir field (shiftAmount, mask) =
      concatMap (trySingleMove shiftAmount) $ splitBits $ field g .&. mask

    -- Try a single move. We are already guaranteed that the piece will land on
    -- the board when we move it in this direction; but we still need to check
    -- that we are not trying to move on top of one of our own pieces. If this
    -- is a legal move, return a list containing just one element, this move:
    -- [ChessMove fromBit toBit]. Otherwise return the empty list.
    trySingleMove :: Int -> Word64 -> [ChessMove]
    trySingleMove shiftAmount b =
      let after :: Word64
          after = shift b shiftAmount
      in if (after .&. whitePieces) == 0
         then [ChessMove b after Nothing]
         else []

    listRayMovesByDir :: Word64 -> (Int, Word64) -> [ChessMove]
    listRayMovesByDir b (shiftAmount, mask) = [ChessMove b x Nothing | x <- stoppingPlaces b]
      where stoppingPlaces c = if c .&. mask == 0
                               then []
                               else let c' = shift c shiftAmount
                                    in if c' .&. whitePieces /= 0
                                       then []
                                       else if c' .&. blackPieces /= 0
                                            then [c']
                                            else c' : stoppingPlaces c'

    listRayMoves :: [(Int, Word64)] -> Word64 -> [ChessMove]
    listRayMoves dirs b = concatMap (listRayMovesByDir b) dirs

    listPawnMoves b =
      let
        dest1 = shiftL b 8
        move1 = ChessMove b dest1 Nothing
        dest2 = shiftL dest1 8
        move2 = ChessMove b dest2 Nothing
        pawnForwardMoves =
          if allPieces .&. dest1 == 0
          then if b .&. 0x000000000000ff00 /= 0  -- pawn is in row 2
                  && allPieces .&. dest2 == 0
               then [move1, move2]
               else [move1]
          else []
        listPawnCaptures (shiftAmount, mask) =
          let dest = shift b shiftAmount
          in if b .&. mask /= 0  &&  dest .&. blackPieces /= 0
             then [ChessMove b dest Nothing]
             else []
        pawnCaptures = concatMap listPawnCaptures whitePawnCaptureDirs
        movesWithoutPromotion = pawnForwardMoves ++ pawnCaptures
        promote (ChessMove orig dest _) =
          [ChessMove orig dest (Just p) | p <- [Queen, Knight, Rook, Bishop]]
      in if b .&. 0x00ff000000000000 /= 0  -- pawn is in row 7
         then concatMap promote movesWithoutPromotion
         else movesWithoutPromotion

    pawnMoves = concatMap listPawnMoves $ splitBits $ wPawns g
    knightMoves = concatMap (listSingleMovesByDir wKnights) knightDirs
    bishopMoves = concatMap (listRayMoves bishopDirs) $ splitBits $ wBishops g
    rookMoves = concatMap (listRayMoves rookDirs) $ splitBits $ wRooks g
    kingMoves = concatMap (listSingleMovesByDir wKing) kingDirs
  in if wKing g == 0
     then []
     else pawnMoves ++ knightMoves ++ bishopMoves ++ rookMoves ++ kingMoves

applyWhiteMove g (ChessMove fromBit toBit promote) =
  let
    applyMoveToBitBoard bits =
      if bits .&. fromBit == 0
      then bits
      else (bits .&. complement fromBit) .|. toBit
    g' = Chessboard {
       bPawns   = bPawns g   .&. complement toBit,
       bKnights = bKnights g .&. complement toBit,
       bBishops = bBishops g .&. complement toBit,
       bRooks   = bRooks g   .&. complement toBit,
       bKing    = bKing g    .&. complement toBit,
       wPawns   = applyMoveToBitBoard $ wPawns g,
       wKnights = applyMoveToBitBoard $ wKnights g,
       wBishops = applyMoveToBitBoard $ wBishops g,
       wRooks   = applyMoveToBitBoard $ wRooks g,
       wKing    = applyMoveToBitBoard $ wKing g,
       whoseTurn = Black}
  in case promote of
    Nothing -> g'
    Just piece ->
      let g'' = g' {wPawns = wPawns g' .&. complement toBit}
      in case piece of
        Queen  -> g'' {wBishops = wBishops g'' .|. toBit,
                       wRooks   = wRooks   g'' .|. toBit}
        Knight -> g'' {wKnights = wKnights g'' .|. toBit}
        Bishop -> g'' {wBishops = wBishops g'' .|. toBit}
        Rook   -> g'' {wRooks   = wRooks   g'' .|. toBit}

instance Game Chessboard where
  type Move Chessboard = ChessMove

  start = Chessboard {
    bPawns   = 0x00ff000000000000,
    bKnights = 0x4200000000000000,
    bBishops = 0x2c00000000000000,
    bRooks   = 0x8900000000000000,
    bKing    = 0x1000000000000000,
    wPawns   = 0x000000000000ff00,
    wKnights = 0x0000000000000042,
    wBishops = 0x000000000000002c,
    wRooks   = 0x0000000000000089,
    wKing    = 0x0000000000000010,
    whoseTurn = White}

  moves g = case whoseTurn g of
    White -> whiteMoves g
    Black -> map flipMove $ whiteMoves $ flipBoard g

  applyMove g move = case whoseTurn g of
    White -> applyWhiteMove g move
    Black -> flipBoard $ applyWhiteMove (flipBoard g) (flipMove move)

  -- Instead of implementing the baroque checkmate rules of chess,
  -- we simply call the game over if an enemy captures your king.
  -- This has one drawback: stalemate is not scored as 0.0.
  scoreFinishedGame g =
    let threatenedKing = if whoseTurn g == White then wKing g else bKing g
    in if threatenedKing == 0 then 1 else 0

-- The first heuristic I attempted was this amazingly bad one:
heuristic0 g = 0

-- Then I tried this almost-as-bad heuristic: just count pieces :)
heuristic1 g = 0.05 * fromIntegral (popCount (allBlackPieces g) - popCount (allWhitePieces g))

-- Now we're at the point where we give the pieces values. The scores here are
-- from Wikipedia, which lists a dozen or more scoring systems to choose
-- from. This one is credited to Hans Berliner.
--     Since we have this bizarre thing where a queen is represented as a
-- bishop and a rook, the code below counts the queen first as a bishop and
-- then as a rook, for 3.33 + 5.1 = 8.43 points. But in Berliner's system a
-- queen is worth 8.80 points, so we add another line of code to add .37 points
-- if the queen is still around.
heuristic g =
    (diff bPawns wPawns
     + 3.2 * diff bKnights wKnights
     + 3.33 * diff bBishops wBishops
     + 5.1 * diff bRooks wRooks
     + 0.37 * diff (\g -> bBishops g .&. bRooks g)
                   (\g -> wBishops g .&. wRooks g))
    * 0.001
  where
    diff blk wht = fromIntegral $ popCount (blk g) - popCount (wht g)

chessAI = bestMoveWithDepthLimit heuristic 4

main = playHumanVsComputer chessAI start
