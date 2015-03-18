module Board where

import Data.Char
import Data.Maybe
import Data.List

data Color = Black
           | White
           | None
             deriving (Eq, Show)

data PieceType = King
               | Queen
               | Rook
               | Bishop
               | Knight
               | Pawn
                 deriving (Eq, Show)

data Piece = Piece Color PieceType
           | Empty
             deriving Eq

data Board = Board [[Piece]] Color (Bool,Bool) (Bool,Bool) Int
             deriving Eq

data Position = Position (Int, Int)
                deriving Eq

data Move = Move Position Position
          | Promotion Position Position PieceType
          | NoMove
            deriving Eq

instance Show Piece where
    show (Piece White piece) = [toUpper $ toChar piece]
    show (Piece Black piece) = [toChar piece]
    show Empty = " "

instance Read Piece where
    readsPrec _ (x:[]) = [(piece, "")]
        where piece = case x of
                        'k' -> Piece Black King
                        'q' -> Piece Black Queen
                        'b' -> Piece Black Bishop
                        'n' -> Piece Black Knight
                        'r' -> Piece Black Rook
                        'p' -> Piece Black Pawn
                        'K' -> Piece White King
                        'Q' -> Piece White Queen
                        'B' -> Piece White Bishop
                        'N' -> Piece White Knight
                        'R' -> Piece White Rook
                        'P' -> Piece White Pawn
                        otherwise -> Empty
    readsPrec _ _ = []

instance Show Board where
    show board@(Board array color (bCastleL, bCastleR) (wCastleL, wCastleR) enPassantFile) = "\n" ++ 
                                                                                             showFileNumbers ++
                                                                                             horizLine ++ 
                                                                                             (foldr (++) "" (map (\(rank, row) -> (showRow rank row) ++ horizLine) (zip [8,7..1] array))) ++
                                                                                             showFileNumbers ++ "\n" ++
                                                                                             (if bCastleL || bCastleR
                                                                                              then "Black can still castle " ++ 
                                                                                                   castleSides (bCastleL, bCastleR) ++ ".\n"
                                                                                              else "") ++
                                                                                             (if wCastleL || wCastleR
                                                                                              then "White can still castle " ++ 
                                                                                                   castleSides (wCastleL, wCastleR) ++ ".\n"
                                                                                              else "") ++
                                                                                             (case enPassantFile of 
                                                                                                (-1) -> ""
                                                                                                otherwise -> "Possible enPassant in file: " ++ [intToFile enPassantFile] ++ "\n") ++
                                                                                             (if isInCheckmate board Black
                                                                                              then "Checkmate. White wins!"
                                                                                              else if isInCheckmate board White
                                                                                                   then "Checkmate. Black wins!"
                                                                                                   else ((if playerIsInCheck board Black
                                                                                                          then "Black is in check!\n"
                                                                                                          else "") ++
                                                                                                         (if playerIsInCheck board White
                                                                                                          then "White is in check!\n"
                                                                                                          else "") ++ 
                                                                                                         (show color) ++ "'s move.\n"))

castleSides :: (Bool, Bool) -> String
castleSides bools = case bools of
                      (True, True) -> "both left and right"
                      (True, False) -> "left only"
                      (False, True) -> "right only"
                      otherwise -> ""

instance Show Position where
    show (Position (file, rank)) = (intToFile file):(show (8 - rank))

instance Read Position where
    readsPrec _ (file:rank:[]) = [(Position (fileToInt file, 8 - (read [rank])), "")]
    readsPrec _ _ = []

instance Show Move where
    show (Move from to) = show from ++ show to
    show (Promotion from to pieceType) = show from ++ show to ++ [toChar pieceType]

instance Read Move where
    readsPrec _ (f1:r1:f2:r2:p:[]) = [((Promotion (read (f1:r1:[])) (read (f2:r2:[])) (read [p])), "")]
    readsPrec _ (f1:r1:f2:r2:[])   = [((Move      (read (f1:r1:[])) (read (f2:r2:[]))           ), "")]
    readsPrec _ _ = []

instance Read PieceType where
    readsPrec _ ('k':[]) = [(King, "")]
    readsPrec _ ('q':[]) = [(Queen, "")]
    readsPrec _ ('r':[]) = [(Rook, "")]
    readsPrec _ ('b':[]) = [(Bishop, "")]
    readsPrec _ ('n':[]) = [(Knight, "")]
    readsPrec _ ('p':[]) = [(Pawn, "")]
    readsPrec _ _ = []

toChar :: PieceType -> Char
toChar King   = 'k'
toChar Queen  = 'q'
toChar Rook   = 'r'
toChar Bishop = 'b'
toChar Knight = 'n'
toChar Pawn   = 'p'

colorOf :: Piece -> Color
colorOf (Piece color _) = color
colorOf Empty = None

typeOf :: Piece -> PieceType
typeOf (Piece _ t) = t

horizLine :: String
horizLine = "   +" ++ (foldr (++) "" (replicate 8 "---+")) ++ "\n"

showRow :: Int -> [Piece] -> String
showRow rank pieceRow = (show rank) ++ "  " ++ 
                        (foldr (++) "" (map (\piece -> "| " ++ (show piece) ++ " ") pieceRow)) ++ 
                        "|  " ++ (show rank) ++ "\n"

showFileNumbers :: String
showFileNumbers = "  " ++ (foldr (++) "" (map (\f -> "   " ++ [f]) ['a'..'h'])) ++ "\n"


backRow :: [PieceType]
backRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

backRowColored :: Color -> [Piece]
backRowColored color = map (\ pieceType -> (Piece color pieceType)) backRow

startingBoard :: Board
startingBoard = Board ([backRowColored Black,
                        replicate 8 (Piece Black Pawn)] ++
                       (replicate 4 (replicate 8 Empty)) ++
                       [replicate 8 (Piece White Pawn),
                        backRowColored White])
                        White (True,True) (True,True) (-1)

pieceAtPosition :: Board -> Position -> Piece
pieceAtPosition (Board array _ _ _ _) (Position (file, rank)) = ((array !! rank) !! file)

fileToInt :: Char -> Int
fileToInt file = (ord file) - (ord 'a')

intToFile :: Int -> Char
intToFile file = chr (file + (ord 'a'))

isOnBoard :: Position -> Bool
isOnBoard (Position (file,rank)) = (file <= 7) && (file >= 0) && (rank <= 7) && (rank >= 0)

pathIsClear :: Board -> Move -> Bool
pathIsClear board (Move from@(Position (fromFile, fromRank)) (Position (toFile, toRank)))
    | typeOf (pieceAtPosition board from) == Knight = True
    | (abs fileRange) == 1 || (abs rankRange) == 1 = True
    | otherwise = foldr (&&) True (map isEmpty (zip fileList rankList))
    where
      isEmpty = (\(f,r) -> (pieceAtPosition board (Position (f,r))) == Empty)
      fileRange = toFile - fromFile
      rankRange = toRank - fromRank
      fileStep = sign fileRange
      rankStep = sign rankRange
      startFile = fromFile + fileStep
      stopFile = toFile - fileStep
      startRank = fromRank + rankStep
      stopRank = toRank - rankStep
      fileList
          | fileStep == 0 = repeat startFile
          | otherwise = [startFile,(startFile+fileStep)..stopFile]
      rankList
          | rankStep == 0 = repeat startRank
          | otherwise = [startRank,(startRank+rankStep)..stopRank]
      sign x
          | x > 0 =  1
          | x < 0 = -1
          | otherwise = 0
pathIsClear board (Promotion from to _) = pathIsClear board (Move from to)

whoseTurn :: Board -> Color
whoseTurn (Board _ color _ _ _) = color

-- This function checks everything except:
--   1) whether or not a move leaves a player in check
--   2) whether or not it's actually this player's turn
isValidMove :: Board -> Move -> Bool
isValidMove board move@(Move from to)
    | isOnBoard from && isOnBoard to = let fromPiece = pieceAtPosition board from
                                           toPiece = pieceAtPosition board to
                                           fromColor = colorOf fromPiece
                                           toColor = colorOf toPiece
                                           fromPieceType = typeOf fromPiece
                                       in fromPiece /= Empty &&
                                          fromColor /= toColor &&
                                          isValidMotion fromPieceType move &&
                                          pathIsClear board move &&
                                          case fromPieceType of
                                            King -> isValidKingMove board move
                                            Pawn -> isValidPawnMove board move
                                            otherwise -> True
    | otherwise = False
isValidMove board (Promotion from to pieceType)
    | isPawnPromotion piece to && pieceType /= King = isValidMove board (Move from to)
    where piece = pieceAtPosition board from

isValidKingMove :: Board -> Move -> Bool
isValidKingMove board@(Board _ _ (blackMayCastleLeft, blackMayCastleRight) (whiteMayCastleLeft, whiteMayCastleRight) _) move@(Move from to)
    | (not $ moveResemblesCastle move) = True
    | color == Black && from == (Position (4, 0)) = if to == (Position (2, 0))
                                                    then blackMayCastleLeft  && castleValidatesWithRookLeft  board color
                                                    else blackMayCastleRight && castleValidatesWithRookRight board color
    | color == White && from == (Position (4, 7)) = if to == (Position (2, 7))
                                                    then whiteMayCastleLeft  && castleValidatesWithRookLeft  board color
                                                    else whiteMayCastleRight && castleValidatesWithRookRight board color
    | otherwise = False
    where color = colorOf $ pieceAtPosition board from

castlingRank :: Color -> Int
castlingRank color = if color == Black
                     then 0
                     else 7

castleValidatesWithRookLeft :: Board -> Color -> Bool
castleValidatesWithRookLeft board color = (not $ positionIsThreatenedBy board (opposite color) (Position (3, rank))) &&
                                          (pieceAtPosition board (Position (0, rank)) == (Piece color Rook)) && 
                                          (pathIsClear board $ Move (Position (0, rank)) (Position (3, rank)))
    where rank = castlingRank color

castleValidatesWithRookRight :: Board -> Color -> Bool
castleValidatesWithRookRight board color = (not $ positionIsThreatenedBy board (opposite color) (Position (5, rank))) &&
                                           (pieceAtPosition board (Position (7, rank)) == (Piece color Rook)) && 
                                           (pathIsClear board $ Move (Position (7, rank)) (Position (5, rank)))
    where rank = castlingRank color

moveResemblesCastle :: Move -> Bool
moveResemblesCastle (Move (Position (fromFile, fromRank)) (Position (toFile, toRank))) = (fromRank == 0 || fromRank == 7) &&
                                                                                         (toRank == fromRank) &&
                                                                                         (fromFile == 4) &&
                                                                                         (toFile == 6 || toFile == 2)

opposite :: Color -> Color
opposite Black = White
opposite White = Black

isValidPawnMove board@(Board _ _ _ _ enPassantFile) move@(Move from@(Position (fromFile, fromRank)) to@(Position (toFile, toRank)))
    | color == Black && rankStep ==  1 = case (abs fileStep) of
                                           0 -> toPiece == Empty
                                           1 -> if toPiece == Empty
                                                then (toRank == 5) && (enPassantFile == toFile)
                                                else colorOf toPiece == opposite color
                                           otherwise -> False
    | color == White && rankStep == -1 = case (abs fileStep) of
                                           0 -> toPiece == Empty
                                           1 -> if toPiece == Empty
                                                then (toRank == 2) && (enPassantFile == toFile)
                                                else colorOf toPiece == opposite color
                                           otherwise -> False
    | color == Black && rankStep ==  2 = toPiece == Empty
    | color == White && rankStep == -2 = toPiece == Empty
    | otherwise = False
    where
          color = colorOf $ pieceAtPosition board from
          fileStep = toFile - fromFile
          rankStep = toRank - fromRank
          toPiece = pieceAtPosition board to
isValidPawnMove board (Promotion from to _) = isValidPawnMove board (Move from to)

isValidMotion :: PieceType -> Move -> Bool
isValidMotion King   move@(Move from@(Position (fromFile, fromRank)) to@(Position (toFile, toRank)))
    | moveResemblesCastle move = True
    | otherwise = (abs (fromRank - toRank)) <= 1 && (abs (fromFile - toFile)) <= 1 && (from /= to)
isValidMotion Queen  move = (isValidMotion Bishop move) || (isValidMotion Rook move)
isValidMotion Bishop (Move (Position (fromFile, fromRank)) (Position (toFile, toRank))) = (abs (fromFile - toFile)) == (abs (fromRank - toRank))
isValidMotion Rook   (Move (Position (fromFile, fromRank)) (Position (toFile, toRank))) = fromRank == toRank || fromFile == toFile
isValidMotion Knight (Move (Position (fromFile, fromRank)) (Position (toFile, toRank)))
              | abs (fromFile - toFile) == 1 = abs (fromRank - toRank) == 2
              | abs (fromFile - toFile) == 2 = abs (fromRank - toRank) == 1
              | otherwise = False
isValidMotion Pawn   (Move (Position (fromFile, fromRank)) (Position (toFile, toRank))) = ((abs (fromRank - toRank)) == 1 && (abs (fromFile - toFile)) <= 1) ||
                                                                                          (fromRank == 1 && toRank == 3 && (fromFile - toFile) == 0) ||
                                                                                          (fromRank == 6 && toRank == 4 && (fromFile - toFile) == 0)
isValidMotion Pawn (Promotion from to _) = isValidMotion Pawn (Move from to)

findKing :: Board -> Color -> Position
findKing (Board array _ _ _ _) color = Position (file, rank)    
    where file = fromJust $ findIndex filePred (array !! rank)
          rank = fromJust $ findIndex rankPred array
          filePred = (== (Piece color King))
          rankPred = ((Piece color King) `elem`)

charToPieceType :: Char -> PieceType
charToPieceType 'k' = King
charToPieceType 'q' = Queen
charToPieceType 'b' = Bishop
charToPieceType 'r' = Rook
charToPieceType 'n' = Knight
charToPieceType 'p' = Pawn
charToPieceType  _  = Queen

makeMove :: Board -> Move -> Maybe Board
makeMove board move@(Move from to)
    | (pieceAtPosition board from == Empty) = Nothing
    | (typeOf $ pieceAtPosition board from) == King && (moveResemblesCastle move) && (playerIsInCheck board color) = Nothing
    | (not $ isInCheckmate board color) && (isValidMove board move) && (not $ playerIsInCheck newBoard color) && (color == (whoseTurn board)) = Just newBoard
    | otherwise = Nothing
    where newBoard = forceMove board move
          color = colorOf $ pieceAtPosition board from
makeMove board (Promotion from to pieceType)
    | pieceType == King = Nothing
    | otherwise = newBoard2 newBoard
    where newBoard = makeMove board (Move from to)
          newBoard2 (Just newBoard') = if (isPawnPromotion piece to)
                                       then Just (setPawnPromotion newBoard' to pieceType)
                                       else Nothing
          newBoard2 Nothing = Nothing
          piece = pieceAtPosition board from

isPawnPromotion :: Piece -> Position -> Bool
isPawnPromotion piece pos@(Position (file, rank))
    | piece == (Piece Black Pawn) && rank == 7 = True
    | piece == (Piece White Pawn) && rank == 0 = True
    | otherwise = False

setPawnPromotion :: Board -> Position -> PieceType -> Board
setPawnPromotion board@(Board a b c d e) pos pieceType = Board (setPositionToPiece pos (Piece color pieceType) a) b c d e
    where color = colorOf $ pieceAtPosition board pos

playerIsInCheck :: Board -> Color -> Bool
playerIsInCheck board color = positionIsThreatenedBy board (opposite color) kingPosition
    where kingPosition = findKing board color

forceMove :: Board -> Move -> Board
forceMove board@(Board array _ blackMayStillCastle whiteMayStillCastle _) move@(Move from@(Position (_, fromRank)) to@(Position (toFile, toRank))) = Board newBoard2 nextTurn (blackCastleLeft,blackCastleRight) (whiteCastleLeft,whiteCastleRight) enPassantFile
    where newBoard1 = setPositionToPiece from Empty $ setPositionToPiece to (pieceAtPosition board from) array
          newBoard2 = if (typeOf fromPiece == King) && (moveResemblesCastle move)
                      then case toFile of
                             2 -> setPositionToPiece (Position (3, toRank)) (Piece (colorOf fromPiece) Rook) $ setPositionToPiece (Position (0, toRank)) Empty newBoard1
                             6 -> setPositionToPiece (Position (5, toRank)) (Piece (colorOf fromPiece) Rook) $ setPositionToPiece (Position (7, toRank)) Empty newBoard1
                      else newBoard1
          nextTurn = opposite $ colorOf $ fromPiece
          blackCastleLeft  = if ((fromPiece == (Piece Black King)) || ((fromPiece == Piece Black Rook) && (from == Position (0, 0))))
                             then False
                             else fst blackMayStillCastle
          blackCastleRight = if ((fromPiece == (Piece Black King)) || ((fromPiece == Piece Black Rook) && (from == Position (7, 0))))
                             then False
                             else snd blackMayStillCastle
          whiteCastleLeft  = if ((fromPiece == (Piece White King)) || ((fromPiece == Piece White Rook) && (from == Position (0, 7))))
                             then False
                             else fst whiteMayStillCastle
          whiteCastleRight = if ((fromPiece == (Piece White King)) || ((fromPiece == Piece White Rook) && (from == Position (7, 7))))
                             then False
                             else snd whiteMayStillCastle
          enPassantFile = if ((typeOf fromPiece) == Pawn) && (abs (toRank - fromRank)) == 2
                          then toFile
                          else (-1)
          fromPiece = pieceAtPosition board from
forceMove board (Promotion from to pieceType) = setPawnPromotion newBoard to pieceType
    where newBoard = forceMove board (Move from to)

setPositionToPiece :: Position -> Piece -> [[Piece]] -> [[Piece]]
setPositionToPiece (Position (file, rank)) piece array = (take rank array) ++ 
                                                         [((take file rankList) ++ 
                                                           [piece] ++ 
                                                           (drop (file + 1) rankList))] ++ 
                                                         (drop (rank + 1) array)
    where rankList = array !! rank

positionIsThreatenedBy :: Board -> Color -> Position -> Bool
positionIsThreatenedBy board color pos = not $ null $ filter (isValidMove board) allMovesToPosition
    where allMovesToPosition = [(Move from pos) | from <- allPiecesOfColor]
          allPiecesOfColor = map snd (allPiecesForColor board color)

allThreatsForColor :: Board -> Color -> [Position]
allThreatsForColor board color = map getTo (allMovesForColor board color)
    where getTo (Move      _ to  ) = to
          getTo (Promotion _ to _) = to

allMovesForColor :: Board -> Color -> [Move]
allMovesForColor board color = foldr (++) [] (map (allMovesForPiece board) (allPiecesForColor board color))

allPiecesForColor :: Board -> Color -> [(Piece, Position)]
allPiecesForColor board color = filter (\((Piece c _),_) -> c == color) (allPieces board)

allPieces :: Board -> [(Piece, Position)]
allPieces board = filter (\(piece, _) -> piece /= Empty) (allSquares board)

allSquares :: Board -> [(Piece, Position)]
allSquares board = [(pieceAtPosition board pos, pos) | pos <- allPositions]

allPositions :: [Position]
allPositions = [Position (file, rank) | file <- [0..7], rank <- [0..7]]

allMovesForPiece :: Board -> (Piece, Position) -> [Move]
allMovesForPiece board (piece, pos) = filter (isValidMove board) $ possibleMoves piece pos

possibleMoves :: Piece -> Position -> [Move]
possibleMoves piece@(Piece _ pieceType) from = expandPawnPromotions $ filter (isValidMotion pieceType) [(Move from to) | to <- allPositions]
    where expandPawnPromotions (move@(Move from to):moves)
              | isPawnPromotion piece to = (Promotion from to Queen):(Promotion from to Knight):(expandPawnPromotions moves)
              | otherwise = move:(expandPawnPromotions moves)
          expandPawnPromotions [] = []

isInCheckmate :: Board -> Color -> Bool
isInCheckmate board color = (playerIsInCheck board color) &&
                            (foldr (&&) True $ map (\move -> playerIsInCheck (forceMove board move) color) $ allMovesForColor board color)

isInStalemate :: Board -> Color -> Bool
isInStalemate board color
    | whoseTurn board /= color = False
    | playerIsInCheck board color = False
    | otherwise = foldr (&&) True $ map (\move -> playerIsInCheck (forceMove board move) color) $ allMovesForColor board color

fenToBoard :: String -> Board
fenToBoard str = Board array whoseTurn (bCastleL, bCastleR) (wCastleL, wCastleR) enPassantFile
    where strings = words str
          array = splitLength 8 $ map (read . (\x -> [x])) $ concat $ filter (\(x:xs) -> x /= '/') $ group $ expandSpaces $ head strings
          expandSpaces (x:xs)
              | x `elem` ['1'..'8'] = (replicate (read [x]) ' ') ++ (expandSpaces xs)
              | otherwise = x:(expandSpaces xs)
          expandSpaces "" = ""
          splitLength n xs@(x:_) = (take n xs):(splitLength n $ drop n xs)
          splitLength _ [] = []
          whoseTurn = case (strings !! 1) of
                        "w" -> White
                        "b" -> Black
          castlingString = strings !! 2
          bCastleL = 'q' `elem` castlingString
          bCastleR = 'k' `elem` castlingString
          wCastleL = 'Q' `elem` castlingString
          wCastleR = 'K' `elem` castlingString
          enPassantString = strings !! 3
          enPassantFile
              | enPassantString == "-" = (-1)
              | otherwise = fileToInt $ head enPassantString

boardToFen :: Board -> String
boardToFen (Board array whoseTurn (bCastleL, bCastleR) (wCastleL, wCastleR) enPassantFile) = arrayString ++ " " ++ turnString ++ " " ++ castlingString ++ " " ++ enPassantString ++ " 0 1"
    where arrayString = concat $ map collapseSpaces $ group $ concat $ intersperse "/" $ map (concat . (map show)) array
          collapseSpaces (' ':xs) = show (1 + length xs)
          collapseSpaces xs       = xs
          turnString = case whoseTurn of
                         White -> "w"
                         Black -> "b"
          castlingString
              | not $ foldr (&&) True [bCastleL, bCastleR, wCastleL, wCastleR] = "-"
              | otherwise = (if wCastleR
                             then "K"
                             else "") ++
                            (if wCastleL
                             then "Q"
                             else "") ++
                            (if bCastleR
                             then "k"
                             else "") ++
                            (if bCastleL
                             then "q"
                             else "")
          enPassantString
              | enPassantFile == (-1) = "-"
              | whoseTurn == White = (intToFile enPassantFile):"6"
              | whoseTurn == Black = (intToFile enPassantFile):"3"

start = Just startingBoard
move b m = makeMove (fromJust b) (read m)
