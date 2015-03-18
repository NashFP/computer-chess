module AI where

import Board
import Data.Maybe

materialValueForType :: PieceType -> Int
materialValueForType Pawn = 1
materialValueForType Knight = 3
materialValueForType Bishop = 3
materialValueForType Rook = 5
materialValueForType Queen = 9

colorMultiplier White = 1
colorMultiplier Black = -1

materialScore :: Board -> Int
materialScore board = sum $ map (\piece -> (materialValue piece) * (numberOf board piece)) allPieces
    where allPieces = [(Piece color pieceType) | color <- [Black, White], pieceType <- [Pawn, Knight, Bishop, Rook, Queen]]

materialValue :: Piece -> Int
materialValue piece@(Piece color pieceType) = (colorMultiplier color) * (materialValueForType pieceType)

numberOf :: Board -> Piece -> Int
numberOf (Board array _ _ _ _) piece = sum $ map (countPieces piece) array
    where countPieces p ps = length $ filter (==p) ps

allLegalMovesForColor :: Board -> Color -> [Move]
allLegalMovesForColor board color = filter isLegalMove (allMovesForColor board color)
    where isLegalMove move = not $ playerIsInCheck (forceMove board move) color

freedomScore :: Board -> Int
freedomScore board = (length $ allLegalMovesForColor board White) - 
                     (length $ allLegalMovesForColor board Black)

-- "Space" is just freedoms that happen to be in enemy territory
spaceScore :: Board -> Int
spaceScore board = (length $ filter isInBlackTerritory (allLegalMovesForColor board White)) - 
                   (length $ filter isInWhiteTerritory (allLegalMovesForColor board Black))
    where isInBlackTerritory move = (rankOfTo move) <  4
          isInWhiteTerritory move = (rankOfTo move) >= 4

rankOfTo :: Move -> Int
rankOfTo (Move      _ (Position (_, rank))  ) = rank
rankOfTo (Promotion _ (Position (_, rank)) _) = rank

castlingScore :: Board -> Int
castlingScore (Board _ _ (bCastleL, bCastleR) (wCastleL, wCastleR) _) = (sum $ map boolToInt [wCastleL, wCastleR]) - 
                                                                        (sum $ map boolToInt [bCastleL, bCastleR])

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

threatScore :: Board -> Int
threatScore board = (threatsToEnemy board White) - (threatsToEnemy board Black)

threatsToEnemy :: Board -> Color -> Int
threatsToEnemy board color = length $ filter (attacksColor board (opposite color)) (allLegalMovesForColor board color)

attacksColor :: Board -> Color -> Move -> Bool
attacksColor board color move = (colorOf $ pieceAtPosition board $ getTo move) == color
    where getTo (Move      _ to  ) = to
          getTo (Promotion _ to _) = to

pawnStructureScore :: Board -> Int
pawnStructureScore _ = 0

checkmateScore :: Board -> Int
checkmateScore board = (boolToInt $ isInCheckmate board Black) - 
                       (boolToInt $ isInCheckmate board White)

selfProtectionScore :: Board -> Int
selfProtectionScore _ = 0

scoreWeights :: [(Board -> Int, Double, String)]
scoreWeights = [(materialScore,          1.0, "Material"),
                (freedomScore,           1.0, "Freedom"),
                (spaceScore,             1.0, "Space"),
                (castlingScore,          1.0, "Castling"),
                (threatScore,            1.0, "Threat"),
                (selfProtectionScore,    1.0, "Self Protection"),
                (pawnStructureScore,     1.0, "Pawn Structure"),
                (checkmateScore,     10000.0, "Checkmate")]

boardScore :: Board -> Double
boardScore board = sum $ map (\(scoreFunc, weight, _) -> weight * (fromIntegral (scoreFunc board))) scoreWeights

scoreBreakdown :: Board -> String
scoreBreakdown board = (concat $ map scoreToString scoreWeights) ++ "Total Score: " ++ (show $ boardScore board) ++ "\n"
    where scoreToString (scoreFunc, weight, name) = name ++ ": " ++ (show (scoreFunc board)) ++ " * " ++ (show weight) ++ " = " ++ (show (weight * fromIntegral (scoreFunc board))) ++ "\n"

data MoveTree = MoveTree Board Move [MoveTree] [MoveTree]

instance Show MoveTree where
    show (MoveTree board _ _ _) = show board

buildMoveTreeList :: MoveTree -> MoveTree
buildMoveTreeList mt@(MoveTree board move [] prevs) = MoveTree board move newList prevs
    where newList = concat $ map (makeMoveTreeFrom mt) $ allMovesForColor board (whoseTurn board)
buildMoveTreeList mt = mt

makeMoveTreeFrom :: MoveTree -> Move -> [MoveTree]
makeMoveTreeFrom mt@(MoveTree board _ _ prevs) move
    | newBoard == Nothing = []
    | otherwise = [MoveTree (fromJust newBoard) move [] (mt:prevs)]
    where newBoard = makeMove board move

score :: MoveTree -> Double
score (MoveTree board _ []  _) = boardScore board
score (MoveTree board _ mts _)
    | whoseTurn board == Black = foldr min (head scores) (tail scores)
    | whoseTurn board == White = foldr max (head scores) (tail scores)
    where scores =  map score mts

optimalMove :: MoveTree -> Move
optimalMove (MoveTree board _ (mt@(MoveTree _ move _ _):[]) _) = move
optimalMove (MoveTree board _ mts _) = fst $ foldr foldFunc (head allMoves) (tail allMoves)
    where allMoves = map (\mt@(MoveTree _ move _ _) -> (move, score mt)) mts
          foldFunc = (\ (m1, s1) (m2, s2) -> if s1 `colorFunc` s2
                                             then (m1, s1)
                                             else (m2, s2))
          colorFunc
              | whoseTurn board == White = (>)
              | whoseTurn board == Black = (<)

selectMove :: MoveTree -> Move -> MoveTree
selectMove mt@(MoveTree board _ []  prevs) move = selectMove (buildMoveTreeList mt) move
selectMove mt@(MoveTree board _ mts prevs) move
    | move `elem` (map (\(MoveTree _ m _ _) -> m) mts) = populateMoveTreeToDepth defaultDepth selectedMT
    where selectedMT = head $ filter (\(MoveTree _ m _ _) -> (m == move)) mts

undo :: MoveTree -> MoveTree
undo (MoveTree _ _ _ (prev:prevs)) = prev
undo mt = mt

startingTree :: MoveTree
startingTree = populateMoveTreeToDepth defaultDepth $ MoveTree startingBoard NoMove [] []

populateMoveTreeToDepth :: Int -> MoveTree -> MoveTree
populateMoveTreeToDepth 0 moveTree = moveTree
populateMoveTreeToDepth depth mt@(MoveTree board move [] prevs) = MoveTree board move (map (populateMoveTreeToDepth (depth - 1)) newList) prevs
    where newList = concat $ map (makeMoveTreeFrom mt) $ allMovesForColor board (whoseTurn board)
populateMoveTreeToDepth depth mt@(MoveTree board move mts prevs) = MoveTree board move (map (populateMoveTreeToDepth (depth - 1)) mts) prevs

defaultDepth = 2

boardOf (MoveTree board _ _ _) = board
