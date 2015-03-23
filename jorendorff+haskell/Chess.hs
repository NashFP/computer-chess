{-# LANGUAGE TypeFamilies #-}

module Chess(Chessboard(..), Suite(..), wholeSuite, ChessMove, ChessColor(..),
             ChessPiece(..), charsToColRow, getPieceAt, unitTests) where

import Minimax
import Data.Bits(Bits, bit, shift, shiftR, shiftL, rotateL, (.&.), (.|.), complement, popCount)
import Data.Int
import Data.Word
import Data.List

data ChessColor = Black | White
  deriving (Eq, Show)

flipColor White = Black
flipColor Black = White

data Suite = Suite {
  pawns   :: Word64,
  knights :: Word64,
  bishops :: Word64,
  rooks   :: Word64,
  king    :: Word64,
  castleK :: Bool,  -- can still castle on king's side
  castleQ :: Bool   -- can still castle on queen's side
  }

data Chessboard = Chessboard {
  black     :: Suite,
  white     :: Suite,
  whoseTurn :: ChessColor,
  -- enPassant is usually 0, but immediately after a pawn advances 2 spaces
  -- from its initial position, enPassant is set to the position the pawn
  -- skipped, the one an opposing pawn would move to in an en passant capture.
  enPassant :: Word64}

wholeSuite s = pawns s .|. knights s .|. bishops s .|. rooks s .|. king s


--- Printing chessboards ------------------------------------------------------

data ChessPiece = King | Queen | Rook | Knight | Bishop | Pawn
  deriving (Eq, Show)

whatsInSuiteAt s bit =
  if pawns s .&. bit /= 0 then Just Pawn
  else if knights s .&. bit /= 0 then Just Knight
  else if bishops s .&. bit /= 0 then Just (if rooks s .&. bit /= 0
                                            then Queen
                                            else Bishop)
  else if rooks s .&. bit /= 0 then Just Rook
  else if king s .&. bit /= 0 then Just King
  else Nothing

getPieceAt g (col, row) =
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
      row r = intersperse ' ' [toChar $ getPieceAt g (c, r) | c <- [0..7] ]
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


--- Reading and writing moves -------------------------------------------------

-- The optional piece at the end here is for pawn promotion.
data ChessMove = ChessMove Word64 Word64 (Maybe ChessPiece)
  deriving Eq

whiteCastles = (
  -- white castling king's side
  (ChessMove 0x0000000000000010 0x0000000000000040 Nothing, 0x0000000000000060, 0x0000000000000020),
  -- white castling queen's side
  (ChessMove 0x0000000000000010 0x0000000000000004 Nothing, 0x000000000000000e, 0x0000000000000008))

blackCastles = (
  -- black castling king's side
  (ChessMove 0x1000000000000000 0x4000000000000000 Nothing, 0x6000000000000000, 0x2000000000000000),
  -- black castling queen's side
  (ChessMove 0x1000000000000000 0x0400000000000000 Nothing, 0x0e00000000000000, 0x0800000000000000))

-- Computes (64 - countLeadingZeros x), assuming x only has a single bit
-- set. Unfortunately countLeadingZeros isn't in Data.Bits in the current
-- stable GHC as of this writing.
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

charsToColRow :: String -> (Int, Int)
charsToColRow [c, r] = (fromEnum c - fromEnum 'a', fromEnum r - fromEnum '1')

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

shiftN b =        shiftL b 8
smearN b =
  let b1 = b  .|. shiftN b
      b2 = b1 .|. shiftL b1 16
  in       b2 .|. shiftL b2 32

shiftS b =        shiftR b 8
smearS b =
  let b1 = b  .|. shiftS b
      b2 = b1 .|. shiftR b1 16
  in       b2 .|. shiftR b2 32

shiftE b =         shiftL b   1 .&. 0xfefefefefefefefe
smearE b =
  let b1 = b  .|.  shiftE b
      b2 = b1 .|. (shiftL b1  2 .&. 0xfcfcfcfcfcfcfcfc)
  in       b2 .|. (shiftL b2  4 .&. 0xf0f0f0f0f0f0f0f0)

shiftW b =         shiftR b   1 .&. 0x7f7f7f7f7f7f7f7f
smearW b =
  let b1 = b  .|.  shiftW b
      b2 = b1 .|. (shiftR b1  2 .&. 0x3f3f3f3f3f3f3f3f)
  in       b2 .|. (shiftR b2  4 .&. 0x0f0f0f0f0f0f0f0f)

shiftNW b =        shiftL b   7 .&. 0x7f7f7f7f7f7f7f7f
smearNW b =
  let b1 = b  .|.  shiftNW b
      b2 = b1 .|. (shiftL b1 14 .&. 0x3f3f3f3f3f3f3f3f)
  in       b2 .|. (shiftL b2 28 .&. 0x0f0f0f0f0f0f0f0f)

shiftSW b =        shiftR b   9 .&. 0x7f7f7f7f7f7f7f7f
smearSW b =
  let b1 = b  .|.  shiftSW b
      b2 = b1 .|. (shiftR b1 18 .&. 0x3f3f3f3f3f3f3f3f)
  in       b2 .|. (shiftR b2 36 .&. 0x0f0f0f0f0f0f0f0f)

shiftNE b =        shiftL b   9 .&. 0xfefefefefefefefe
smearNE b =
  let b1 = b  .|.  shiftNE b
      b2 = b1 .|. (shiftL b1 18 .&. 0xfcfcfcfcfcfcfcfc)
  in       b2 .|. (shiftL b2 36 .&. 0xf0f0f0f0f0f0f0f0)

shiftSE b =        shiftR b   7 .&. 0xfefefefefefefefe
smearSE b =
  let b1 = b  .|.  shiftSE b
      b2 = b1 .|. (shiftR b1 14 .&. 0xfcfcfcfcfcfcfcfc)
  in       b2 .|. (shiftR b2 28 .&. 0xf0f0f0f0f0f0f0f0)

-- Exhaustively test the shiftDir functions
unitTests :: [Either String String]
unitTests = map checkDir dirs
  where
    dirs :: [(String, Int, Int, Word64 -> Word64, Word64 -> Word64)]
    dirs = [( "E",  1,  0, shiftE,  smearE),
            ("NE",  1,  1, shiftNE, smearNE),
            ( "N",  0,  1, shiftN,  smearN),
            ("NW", -1,  1, shiftNW, smearNW),
            ( "W", -1,  0, shiftW,  smearW),
            ("SW", -1, -1, shiftSW, smearSW),
            ( "S",  0, -1, shiftS,  smearS),
            ("SE",  1, -1, shiftSE, smearSE)]
    squares = [(col, row) | col <- [0..7], row <- [0..7]]
    slowShift (_, dx, dy, _, _) squares =
      [(col', row')
      | (col, row) <- squares,
        let col' = col + dx,
        0 <= col' && col' < 8,
        let row' = row + dy,
        0 <= row' && row' < 8]
    slowSmear :: (String, Int, Int, Word64 -> Word64, Word64 -> Word64) -> [(Int, Int)] -> [(Int, Int)]
    slowSmear dir squares =
      foldr union [] $ take 8 $ iterate (slowShift dir) squares
    squareToBit :: (Int, Int) -> Word64
    squareToBit (col, row) = bit (8 * row + col)
    squaresToBits squares = foldr (.|.) 0 (map squareToBit squares)
    checkDir dir@(name, _, _, shift, smear) = do
      mapM_ (\square -> let expected = squaresToBits (slowShift dir [square])
                            actual = shift (squareToBit square)
                        in if actual == expected
                           then Right "pass"
                           else Left ("shift" ++ name ++ " " ++ show square ++ ": " ++
                                      "got " ++ show actual ++ ", expected " ++ show expected)) squares
      mapM_ (\square -> let expected = squaresToBits (slowSmear dir [square])
                            actual = smear (squareToBit square)
                        in if actual == expected
                           then Right "pass"
                           else Left ("smear" ++ name ++ " " ++ show square ++ ": " ++
                                      "got " ++ show actual ++ ", expected " ++ show expected)) squares
      return $ "shift" ++ name ++ " and smear" ++ name


{-# INLINE squareIsAttackedOnRay #-}
squareIsAttackedOnRay shiftDir smearDir square attackers others =
  let ray = smearDir (shiftDir square)
      attackerShadow = smearDir (attackers .&. ray)
      otherShadow = smearDir (others .&. ray)
  in attackerShadow /= 0 &&  -- pure optimization to avoid computing otherShadow
     attackerShadow .&. complement otherShadow /= 0

squareIsAttackedByRook square rooks others =
  squareIsAttackedOnRay shiftN smearN square rooks others
  || squareIsAttackedOnRay shiftS smearS square rooks others
  || squareIsAttackedOnRay shiftW smearW square rooks others
  || squareIsAttackedOnRay shiftE smearE square rooks others

squareIsAttackedByBishop square bishops others =
  squareIsAttackedOnRay shiftNE smearNE square bishops others
  || squareIsAttackedOnRay shiftNW smearNW square bishops others
  || squareIsAttackedOnRay shiftSW smearSW square bishops others
  || squareIsAttackedOnRay shiftSE smearSE square bishops others

knightMask = 0x0442800000028440

squareIsAttackedByKnight square knights =
  rotateL knightMask (log2OfBit square) .&. knights /= 0   -- quick test with false positives
  && or [square .&. shift (knights .&. mask) shiftAmount /= 0 | (shiftAmount, mask) <- knightDirs]

squareIsAttackedByWhitePawn square whitePawns =
     square .&. shift (whitePawns .&. maskNE) shiftAmountNE /= 0
  || square .&. shift (whitePawns .&. maskNW) shiftAmountNW /= 0
  where
    maskNE = 0x007f7f7f7f7f7f7f
    shiftAmountNE = 9
    maskNW = 0x00fefefefefefefe
    shiftAmountNW =  7

squareIsAttackedByBlackPawn square blackPawns =
     square .&. shift (blackPawns .&. maskSW) shiftAmountSW /= 0
  || square .&. shift (blackPawns .&. maskSE) shiftAmountSE /= 0
  where
    maskSW = 0xfefefefefefefe00
    shiftAmountSW = -9
    maskSE = 0x7f7f7f7f7f7f7f00
    shiftAmountSE = -7

kingMask = 0x8380000000000382

squareIsAttackedByKing square king =
     square == shiftE  king
  || square == shiftNE king
  || square == shiftN  king
  || square == shiftNW king
  || square == shiftW  king
  || square == shiftSW king
  || square == shiftS  king
  || square == shiftSE king

-- Given the board 'g' and a bit 'square', return true if any of 'color's
-- pieces are attacking that square.
squareIsThreatenedBy g square color =
  let (attackers, defenders, squareIsAttackedByPawn) = case color of
        White -> (white g, black g, squareIsAttackedByWhitePawn)
        Black -> (black g, white g, squareIsAttackedByBlackPawn)
      allPieces = wholeSuite attackers .|. wholeSuite defenders
      Suite {rooks=aRooks,
             bishops=aBishops,
             knights=aKnights,
             pawns=aPawns,
             king=aKing} = attackers
  in squareIsAttackedByRook square aRooks (allPieces .&. complement aRooks)
     || squareIsAttackedByBishop square aBishops (allPieces .&. complement aBishops)
     || squareIsAttackedByKnight square aKnights
     || squareIsAttackedByPawn square aPawns
     || squareIsAttackedByKing square aKing

  -- let hypothetical = g {whoseTurn = color}
  -- in any (\(ChessMove _ to _ ) -> to == square) $ naiveMoves hypothetical

rank2 = 0x000000000000ff00  -- white pawns start here
rank7 = 0x00ff000000000000  -- black pawns start here

-- Return the list of all moves, without eliminating moves that leave
-- the current player's king in check. Moves where a piece would capture
-- an opposing king are also included (indeed we rely on this).
naiveMoves :: Chessboard -> [ChessMove]
naiveMoves g =
  let
    (friendly, enemy, pawnHomeRow, pawnPromoteRow, pawnShiftAmount, pawnCaptureDirs, castles) =
      case whoseTurn g of
        White -> (white g, black g, rank2, rank7, 8, whitePawnCaptureDirs, whiteCastles)
        Black -> (black g, white g, rank7, rank2, -8, blackPawnCaptureDirs, blackCastles)

    friendlyPieces = wholeSuite friendly
    enemyPieces = wholeSuite enemy
    enemyPiecesPlusEnPassant = enemyPieces .|. enPassant g
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
          in if b .&. mask /= 0  &&  dest .&. enemyPiecesPlusEnPassant /= 0
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
    castlingMoves =
      let enemyColor = flipColor $ whoseTurn g
          ((moveK, maskK, safeK), (moveQ, maskQ, safeQ)) = castles
          castlingMoveIfLegal canStillCastleBit move mask safe =
            if canStillCastleBit friendly
               && allPieces .&. mask == 0
               && not (squareIsThreatenedBy g safe enemyColor)
            then [move]
            else []
      in castlingMoveIfLegal castleK moveK maskK safeK ++
         castlingMoveIfLegal castleQ moveQ maskQ safeQ
  in if my king == 0
     then []
     else pawnMoves ++ knightMoves ++ bishopMoves ++ rookMoves ++ kingMoves ++ castlingMoves

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

applyChessMove g (ChessMove fromBit toBit promote) =
  let
    applyMoveToBitBoard bits =
      if bits .&. fromBit == 0
      then bits
      else (bits .&. complement fromBit) .|. toBit

    (friends, enemies, forwardShift, castleKSpots, castleQSpots) = case whoseTurn g of
      White -> (white g, black g,  8, 0x0000000000000090, 0x0000000000000011)
      Black -> (black g, white g, -8, 0x9000000000000000, 0x1100000000000000)

    enemies' =
      let Suite {pawns = p, knights = n, bishops = b, rooks = r, king = k} = enemies
          mask = complement toBit
          enPassantTarget = enPassant g
      in if (p .|. n .|. b .|. r) .&. toBit /= 0  -- ordinary capture
         then enemies {
           pawns   = p .&. mask,
           knights = n .&. mask,
           bishops = b .&. mask,
           rooks   = r .&. mask}
         else if toBit == enPassantTarget  &&  pawns friends .&. fromBit /= 0  -- en passant capture
              then enemies {pawns = p .&. complement (shift toBit (-forwardShift))}
              else enemies -- no capture: save some memory by reusing this suite

    friends' = finishCastling $ applyPromotion $ Suite {
      pawns   = applyMoveToBitBoard $ pawns friends,
      knights = applyMoveToBitBoard $ knights friends,
      bishops = applyMoveToBitBoard $ bishops friends,
      rooks   = applyMoveToBitBoard $ rooks friends,
      king    = applyMoveToBitBoard $ king friends,
      castleK = castleK friends  &&  fromBit .&. castleKSpots == 0,
      castleQ = castleQ friends  &&  fromBit .&. castleQSpots == 0}

    finishCastling side =
      if fromBit /= king friends
      then side
      else if toBit == shiftL fromBit 2
           then side {rooks = (rooks side .&. complement (shiftL fromBit 3)) .|. shiftL fromBit 1}
           else if toBit == shiftR fromBit 2
                then side {rooks = (rooks side .&. complement (shiftR fromBit 4)) .|. shiftR fromBit 1}
                else side

    applyPromotion side = case promote of
      Nothing -> side
      Just piece ->
        let side' = side {pawns = pawns side .&. complement toBit}
        in case piece of
             Queen  -> side' {bishops = bishops side' .|. toBit,
                              rooks   = rooks   side' .|. toBit}
             Knight -> side' {knights = knights side' .|. toBit}
             Bishop -> side' {bishops = bishops side' .|. toBit}
             Rook   -> side' {rooks   = rooks   side' .|. toBit}

    enPassant' = if fromBit .&. pawns friends /= 0
                    && toBit == shift fromBit (2 * forwardShift)
                 then shift fromBit forwardShift
                 else 0

  in case whoseTurn g of
       White -> Chessboard {white = friends', black = enemies', whoseTurn = Black, enPassant = enPassant'}
       Black -> Chessboard {white = enemies', black = friends', whoseTurn = White, enPassant = enPassant'}

instance Game Chessboard where
  type Move Chessboard = ChessMove

  start = Chessboard {
    black = Suite {
      pawns   = 0x00ff000000000000,
      knights = 0x4200000000000000,
      bishops = 0x2c00000000000000,
      rooks   = 0x8900000000000000,
      king    = 0x1000000000000000,
      castleK = True,
      castleQ = True},
    white = Suite {
      pawns   = 0x000000000000ff00,
      knights = 0x0000000000000042,
      bishops = 0x000000000000002c,
      rooks   = 0x0000000000000089,
      king    = 0x0000000000000010,
      castleK = True,
      castleQ = True},
    whoseTurn = White,
    enPassant = 0}

  moves = legalMoves

  applyMove = applyChessMove

  scoreFinishedGame g =
    let side = case whoseTurn g of
          White -> white g
          Black -> black g
    in if squareIsThreatenedBy g (king side) (flipColor $ whoseTurn g) then 1 else 0
