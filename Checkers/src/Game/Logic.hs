-- **************************************************************************************
-- * Checkers 1.0 - The spanish checkers game implemented in Haskell.                   *
-- * Copyright (C) 2007  Gorka Suárez García & Enrique López Mañas                      *
-- *                                                                                    *
-- * This program is free software; you can redistribute it and/or                      *
-- * modify it under the terms of the GNU General Public License                        *
-- * as published by the Free Software Foundation; either version 2                     *
-- * of the License, or (at your option) any later version.                             *
-- *                                                                                    *
-- * This program is distributed in the hope that it will be useful,                    *
-- * but WITHOUT ANY WARRANTY; without even the implied warranty of                     *
-- * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                      *
-- * GNU General Public License for more details.                                       *
-- *                                                                                    *
-- * You should have received a copy of the GNU General Public License                  *
-- * along with this program; if not, write to the Free Software                        *
-- * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.    *
-- **************************************************************************************
module Game.Logic where

-- **************************************************************************************
-- * External modules                                                                   *
-- **************************************************************************************
import Game.Data

-- *********************************************************************************
--                                       Types
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Type name: Vector
-- Description: This represents a vector inside the board.
-- ---------------------------------------------------------------------------------
type Vector = (Int, Int)

-- ---------------------------------------------------------------------------------
-- Type name: RelCoords
-- Description: This represents a relative coordinates inside the board.
-- ---------------------------------------------------------------------------------
type RelCoords = (Int, Int)

-- ---------------------------------------------------------------------------------
-- Type name: NodePiece
-- Description: This represents a piece and all its posible moves.
-- ---------------------------------------------------------------------------------
type NodePiece = (Position, [TreeMoves])

-- ---------------------------------------------------------------------------------
-- Type name: TreeMoves
-- Description: This new type is to create a tree with all the moves in a turn.
-- ---------------------------------------------------------------------------------
data TreeMoves = TMRoot [NodePiece] | TMNode Position [TreeMoves]
     deriving (Eq, Show, Read)


-- *********************************************************************************
--                                       Data
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: getAllNormalMoves
-- Description: Gets all the moves of a normal piece.
-- ---------------------------------------------------------------------------------
getAllNormalMoves::PColor -> [RelCoords]
getAllNormalMoves White = [(1, -1), (1, 1), (2, -2), (2, 2)]
getAllNormalMoves Black = [(-1, -1), (-1, 1), (-2, -2), (-2, 2)]

-- ---------------------------------------------------------------------------------
-- Function: getAllKingMoves
-- Description: Gets all the moves of a king piece.
-- ---------------------------------------------------------------------------------
getAllKingMoves::[RelCoords]
getAllKingMoves = [(1, -1), (2, -2), (3, -3), (4, -4), (5, -5), (6, -6), (7, -7),
                   (1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7),
                   (-1, -1), (-2, -2), (-3, -3), (-4, -4), (-5, -5), (-6, -6), (-7, -7),
                   (-1, 1), (-2, 2), (-3, 3), (-4, 4), (-5, 5), (-6, 6), (-7, 7)]

-- ---------------------------------------------------------------------------------
-- Function: getAllMoves
-- Description: Gets all the moves of a piece.
-- ---------------------------------------------------------------------------------
getAllMoves::PType -> PColor -> [RelCoords]
getAllMoves Normal color = getAllNormalMoves color
getAllMoves King _ = getAllKingMoves


-- *********************************************************************************
--                                       Util
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: getRelCoords
-- Description: Gets a relative coordinates from a move.
-- ---------------------------------------------------------------------------------
getRelCoords::Position -> Position -> RelCoords
getRelCoords (orow, ocol) (drow, dcol) = (drow - orow, dcol - ocol)

-- ---------------------------------------------------------------------------------
-- Function: getVector
-- Description: Gets a directional vector from a relative coordinates.
-- ---------------------------------------------------------------------------------
getVector::RelCoords -> Vector
getVector (rrow, rcol) = (div rrow factor, div rcol factor)
    where factor = abs rrow

-- ---------------------------------------------------------------------------------
-- Function: calcPrevSquare
-- Description: Gets the position of the previous square to the destine.
-- ---------------------------------------------------------------------------------
calcPrevSquare::Position -> Vector -> Position
calcPrevSquare (drow, dcol) (vrow, vcol) = (drow - vrow, dcol - vcol)

-- ---------------------------------------------------------------------------------
-- Function: checkSquareColor
-- Description: Checks if a square have a piece with the same color.
-- ---------------------------------------------------------------------------------
checkSquareColor::Square -> PColor -> Bool
checkSquareColor (SquarePiece (c, _)) color = (c == color)
checkSquareColor x _ = False

-- ---------------------------------------------------------------------------------
-- Function: checkEnemySquare
-- Description: Checks if the square have an enemy piece.
-- ---------------------------------------------------------------------------------
checkEnemySquare::PColor -> Square -> Bool
checkEnemySquare color (SquarePiece (c, _)) = (color /= c)
checkEnemySquare color x = False


-- *********************************************************************************
--                                  Validate origin
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: validateOrigin
-- Description: Validates that the origin position is a piece of a player.
-- ---------------------------------------------------------------------------------
validateOrigin::Board -> PColor -> Position -> Bool
validateOrigin board color (row, col) = checkSquareColor square color
    where square = getBoardSquare board row col


-- *********************************************************************************
--                                   Validate move
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: getPrevSquare
-- Description: Gets the previous square to the destination one.
-- ---------------------------------------------------------------------------------
getPrevSquare::Board -> Position -> RelCoords -> Square
getPrevSquare board (drow, dcol) (rrow, rcol) = getBoardSquare board nrow ncol
    where (vrow, vcol) = getVector (rrow, rcol)
          (nrow, ncol) = calcPrevSquare (drow, dcol) (vrow, vcol)

-- ---------------------------------------------------------------------------------
-- Function: validateNormalMove
-- Description: Validates the move that a normal piece wants to do.
-- ---------------------------------------------------------------------------------
validateNormalMove::Board -> PColor -> Position -> Position -> Bool
validateNormalMove board color (orow, ocol) (drow, dcol)
    | validMove && ((abs rrow) == 1) = True
    | validMove && ((abs rrow) == 2) = checkEnemySquare color prevSquare
    | otherwise = False
    where (rrow, rcol) = getRelCoords (orow, ocol) (drow, dcol)
          validMove = or [(rrow, rcol) == x | x <- (getAllNormalMoves color)]
          prevSquare = getPrevSquare board (drow, dcol) (rrow, rcol)

-- ---------------------------------------------------------------------------------
-- Function: countPieces
-- Description: Counts the enemy pieces between the origin and the destination.
-- ---------------------------------------------------------------------------------
countPieces::Board -> PColor -> Vector -> Position -> Position -> Int -> Int
countPieces board color (vr, vc) (r, c) (dr, dc) count
    | notEnded && empty = countPieces board color (vr, vc) (r+vr, c+vc) (dr, dc) count
    | notEnded && sameColor = (-1)
    | notEnded = countPieces board color (vr, vc) (r+vr, c+vc) (dr, dc) (count+1)
    | otherwise = count
    where notEnded = (r /= dr) && (c /= dc)
          square = getBoardSquare board r c
          empty = (square == SquareEmpty)
          sameColor = checkSquareColor square color

-- ---------------------------------------------------------------------------------
-- Function: validateKingMove
-- Description: Validates the move that a king piece wants to do.
-- ---------------------------------------------------------------------------------
validateKingMove::Board -> PColor -> Position -> Position -> Bool
validateKingMove board color (orow, ocol) (drow, dcol)
    | validMove && (0 == count) = True
    | validMove && (count == 1) = checkEnemySquare color prevSquare
    | otherwise = False
    where (rrow, rcol) = getRelCoords (orow, ocol) (drow, dcol)
          (vrow, vcol) = getVector (rrow, rcol)
          validMove = or [(rrow, rcol) == x | x <- getAllKingMoves]
          count = countPieces board color (vrow, vcol) (orow+vrow, ocol+vcol) (drow, dcol) 0
          prevSquare = getPrevSquare board (drow, dcol) (rrow, rcol)

-- ---------------------------------------------------------------------------------
-- Function: validateMoveEx
-- Description: Validates the move the player wants to do.
-- ---------------------------------------------------------------------------------
validateMoveEx::PType -> Board -> PColor -> Position -> Position -> Bool
validateMoveEx King board pcolor orig dest = validateKingMove board pcolor orig dest
validateMoveEx Normal board pcolor orig dest = validateNormalMove board pcolor orig dest

-- ---------------------------------------------------------------------------------
-- Function: validateMove
-- Description: Validates the move the player wants to do.
-- ---------------------------------------------------------------------------------
validateMove::Board -> Position -> Position -> Bool
validateMove board (orow, ocol) (drow, dcol)
    | destEmpty = validateMoveEx ptype board pcolor (orow, ocol) (drow, dcol)
    | otherwise = False
    where destEmpty = (SquareEmpty == (getBoardSquare board drow dcol))
          (SquarePiece (pcolor, ptype)) = getBoardSquare board orow ocol


-- *********************************************************************************
--                                    Move piece
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: killPiece
-- Description: Kills the previous piece to the destination move.
-- ---------------------------------------------------------------------------------
killPiece::Board -> Position -> Vector -> Board
killPiece board (dr, dc) (vr, vc) = setBoardSquare board r c SquareEmpty
    where (r, c) = calcPrevSquare (dr, dc) (vr, vc)

-- ---------------------------------------------------------------------------------
-- Function: transformSquare
-- Description: This transforms a normal piece into a king.
-- ---------------------------------------------------------------------------------
transformSquare::Square -> Position -> Square
transformSquare (SquarePiece (White, Normal)) (8, _) = (SquarePiece (White, King))
transformSquare (SquarePiece (Black, Normal)) (1, _) = (SquarePiece (Black, King))
transformSquare square dest = square

-- ---------------------------------------------------------------------------------
-- Function: makeMove
-- Description: Moves a piece from an origin to a destination.
-- ---------------------------------------------------------------------------------
makeMove::Board -> Position -> Position -> Board
makeMove board (orow, ocol) (drow, dcol) = finalBoard
    where square = transformSquare (getBoardSquare board orow ocol) (drow, dcol)
          newBoard = setBoardSquare (setBoardSquare board orow ocol SquareEmpty) drow dcol square
          (rrow, rcol) = getRelCoords (orow, ocol) (drow, dcol)
          (vrow, vcol) = getVector (rrow, rcol)
          finalBoard = if (abs rrow) > 1 && (abs rcol) > 1 then killPiece newBoard (drow, dcol) (vrow, vcol)
                                                           else newBoard


-- *********************************************************************************
--                                    Still move
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: getAllBoardPositions
-- Description: Gets all the board positions.
-- ---------------------------------------------------------------------------------
getAllBoardPositions::[Position]
getAllBoardPositions = [(r, c) | r <- [1..8], c <- [1..8],
                        (checkEvenCoords r c) || (checkOddCoords r c)]

-- ---------------------------------------------------------------------------------
-- Function: countTotalPiecesEx
-- Description: Counts the total number of pieces of a color.
-- ---------------------------------------------------------------------------------
countTotalPiecesEx::Board -> PColor -> PType -> Int
countTotalPiecesEx board color ptype = length [x | x <- xs, x == (SquarePiece (color, ptype))]
    where xs = [(getBoardSquare board r c) | (r, c) <- getAllBoardPositions]

-- ---------------------------------------------------------------------------------
-- Function: countTotalPieces
-- Description: Counts the total number of pieces of a color.
-- ---------------------------------------------------------------------------------
countTotalPieces::Board -> PColor -> Int
countTotalPieces board color = length [x | x <- xs, checkSquareColor x color]
    where xs = [(getBoardSquare board r c) | (r, c) <- getAllBoardPositions]

-- ---------------------------------------------------------------------------------
-- Function: getAllPieceMovesEx
-- Description: Gets all the posible moves of a piece.
-- ---------------------------------------------------------------------------------
getAllPieceMovesEx::Board -> Position -> Square -> [(Position, Position)]
getAllPieceMovesEx board (r, c) (SquarePiece (pcolor, ptype))
  = [((r, c), (r + rr, c + rc)) |
     (rr, rc) <- getAllMoves ptype pcolor,
     validateMove board (r, c) (r + rr, c + rc)]
getAllPieceMovesEx _ _ _ = []

-- ---------------------------------------------------------------------------------
-- Function: getAllPieceMoves
-- Description: Gets all the posible moves of a piece.
-- ---------------------------------------------------------------------------------
getAllPieceMoves::Board -> Position -> [(Position, Position)]
getAllPieceMoves board (r, c) = getAllPieceMovesEx board (r, c) (getBoardSquare board r c)

-- ---------------------------------------------------------------------------------
-- Function: oppColor
-- Description: Gets the opposite color.
-- ---------------------------------------------------------------------------------
oppColor::PColor -> PColor
oppColor White = Black
oppColor Black = White

-- ---------------------------------------------------------------------------------
-- Function: killNormalCondition
-- Description: Gets if a normal piece can check the stillKillMove condition.
-- ---------------------------------------------------------------------------------
killNormalCondition::Position -> Position -> Square -> Bool
killNormalCondition (r, c) dest (SquarePiece (_, Normal))
  = ((abs rrow) == 2)
    where (rrow, rcol) = getRelCoords (r, c) dest
killNormalCondition _ _ _ = True

-- ---------------------------------------------------------------------------------
-- Function: stillKillMove
-- Description: Gets if a piece can still move to kill an enemy.
-- ---------------------------------------------------------------------------------
stillKillMove::Board -> PColor -> Position -> Position -> Bool
stillKillMove board color (r, c) orig = normalCondition && (validMoves > 0)
    where numActEne = countTotalPieces board (oppColor color)
          validMoves = length [(orig, dest) | (orig, dest) <- (getAllPieceMoves board orig),
                               numActEne > (countTotalPieces (makeMove board orig dest)
                                                             (oppColor color))]
          normalCondition = killNormalCondition (r, c) orig (getBoardSquare board r c)


-- *********************************************************************************
--                              Artificial Intelligence
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: evalColorSide
-- Description: Calculates the value of a color side.
-- ---------------------------------------------------------------------------------
evalColorSide::Board -> PColor -> Int
evalColorSide board color = (numKing * 10 + numNormal)
    where numNormal = countTotalPiecesEx board color Normal
          numKing = countTotalPiecesEx board color King

-- ---------------------------------------------------------------------------------
-- Function: evalBoardBlack
-- Description: Evaluator for the computer when the player is the black side.
-- ---------------------------------------------------------------------------------
evalBoardBlack::Board -> Int
evalBoardBlack board = (evalColorSide board White) - (evalColorSide board Black)

-- ---------------------------------------------------------------------------------
-- Function: evalBoardWhite
-- Description: Evaluator for the computer when the player is the white side.
-- ---------------------------------------------------------------------------------
evalBoardWhite::Board -> Int
evalBoardWhite board = (evalColorSide board Black) - (evalColorSide board White)

-- ---------------------------------------------------------------------------------
-- Function: getPiecesPositions
-- Description: Gets all the positions of the pieces of a color.
-- ---------------------------------------------------------------------------------
getPiecesPositions::Board -> PColor -> [Position]
getPiecesPositions board color = [(r, c) | (r, c) <- getAllBoardPositions,
                                  checkSquareColor (getBoardSquare board r c) color]

-- ---------------------------------------------------------------------------------
-- Function: getNodeNextKill
-- Description: Gets all the kill moves inside a list of nodes.
-- ---------------------------------------------------------------------------------
getNodeNextKill::Board -> PColor -> Position -> [TreeMoves]
getNodeNextKill board color orig
  = [TMNode dest (getNodeNextKill (makeMove board orig dest) color dest) |
     (_, dest) <- (getAllPieceMoves board orig),
     (numActEne > (countTotalPieces (makeMove board orig dest) (oppColor color)))]
    where numActEne = countTotalPieces board (oppColor color)

-- ---------------------------------------------------------------------------------
-- Function: getNodeNextKillEx
-- Description: Gets all the kill moves inside a list of nodes.
-- ---------------------------------------------------------------------------------
getNodeNextKillEx::Board -> PColor -> Position -> Position -> [TreeMoves]
getNodeNextKillEx board color (or, oc) (dr, dc)
  = if normalCondition then (getNodeNextKill board color (dr, dc))
                       else []
    where normalCondition = killNormalCondition (or, oc) (dr, dc)
                                                (getBoardSquare board dr dc)

-- ---------------------------------------------------------------------------------
-- Function: getNodeNextAll
-- Description: Gets all the moves inside a list of nodes.
-- ---------------------------------------------------------------------------------
getNodeNextAll::Board -> PColor -> Position -> [TreeMoves]
getNodeNextAll board color orig = [TMNode dest (getNodeNextKillEx (makeMove board orig dest) color orig dest) |
                                   (_, dest) <- (getAllPieceMoves board orig)]

-- ---------------------------------------------------------------------------------
-- Function: getNodePiece
-- Description: Gets all the node pieces in the root of the tree.
-- ---------------------------------------------------------------------------------
getNodePiece::Board -> PColor -> [Position] -> [NodePiece]
getNodePiece board color [] = []
getNodePiece board color (x:xs) = if nextLevels == [] then nextPieces
                                                      else (x, nextLevels) : nextPieces
    where nextLevels = getNodeNextAll board color x
          nextPieces = getNodePiece board color xs

-- ---------------------------------------------------------------------------------
-- Function: getTreeMoves
-- Description: Gets a tree with all the moves in this turn.
-- ---------------------------------------------------------------------------------
getTreeMoves::Board -> PColor -> TreeMoves
getTreeMoves board color = TMRoot (getNodePiece board color positions)
    where positions = getPiecesPositions board color

-- ---------------------------------------------------------------------------------
-- Function: getNodeTotalMoves
-- Description: Gets the total number of moves from a NodePiece.
-- ---------------------------------------------------------------------------------
getNodeTotalMoves::NodePiece -> Int
getNodeTotalMoves (_, []) = 1
getNodeTotalMoves (_, xs) = sum [getTreeTotalMoves x | x <- xs]

-- ---------------------------------------------------------------------------------
-- Function: getTreeTotalMoves
-- Description: Gets the total number of moves from a TreeMoves.
-- ---------------------------------------------------------------------------------
getTreeTotalMoves::TreeMoves -> Int
getTreeTotalMoves (TMNode _ []) = 1
getTreeTotalMoves (TMNode _ xs) = sum [getTreeTotalMoves x | x <- xs]
getTreeTotalMoves (TMRoot []) = 0
getTreeTotalMoves (TMRoot xs) =  sum [getNodeTotalMoves x | x <- xs]

-- ---------------------------------------------------------------------------------
-- Function: getMoveFromNodes
-- Description: Gets a move from a node of a TreeMoves.
-- ---------------------------------------------------------------------------------
getMoveFromNodes::[TreeMoves] -> Int -> [Position] -> [Int] -> [Position]
getMoveFromNodes [] 1 accum _ = accum
getMoveFromNodes [] i _ _ = []
getMoveFromNodes ((TMNode pos subnodes):nodes) i accum (x:xs)
    | (i > x) = getMoveFromNodes nodes (i - x) accum xs
    | otherwise = getMoveFromNodes subnodes i (accum++[pos]) listTotMov
    where listTotMov = [getTreeTotalMoves x | x <- subnodes]

-- ---------------------------------------------------------------------------------
-- Function: getMoveFromNodePieces
-- Description: Gets a move from a NodePiece of a TreeMoves.
-- ---------------------------------------------------------------------------------
getMoveFromNodePieces::[NodePiece] -> Int -> [Int] -> [Position]
getMoveFromNodePieces ((pos, nodes):nps) i (x:xs)
    | (i > x) = getMoveFromNodePieces nps (i - x) xs
    | otherwise = getMoveFromNodes nodes i [pos] listTotMov
    where listTotMov = [getTreeTotalMoves x | x <- nodes]

-- ---------------------------------------------------------------------------------
-- Function: getMoveFromRoot
-- Description: Gets a move from the root of a TreeMoves.
-- ---------------------------------------------------------------------------------
getMoveFromRoot::TreeMoves -> Int -> [Position]
getMoveFromRoot (TMRoot []) i = []
getMoveFromRoot (TMRoot xs) i = getMoveFromNodePieces xs i listTotMov
    where listTotMov = [getNodeTotalMoves x | x <- xs]

-- ---------------------------------------------------------------------------------
-- Function: getMoveFromTree
-- Description: Gets a move from a TreeMoves.
-- ---------------------------------------------------------------------------------
getMoveFromTree::TreeMoves -> Int -> [Position]
getMoveFromTree tree i
    | validIndex = getMoveFromRoot tree i
    | otherwise = []
    where totalMoves = getTreeTotalMoves tree
          validIndex = (1 <= i) && (i <= totalMoves)

-- ---------------------------------------------------------------------------------
-- Function: initialAlpha
-- Description: A constant for the initial alpha in the minimax algorithm.
-- ---------------------------------------------------------------------------------
initialAlpha::Int
initialAlpha = (-1000000)

-- ---------------------------------------------------------------------------------
-- Function: initialBeta
-- Description: A constant for the initial beta in the minimax algorithm.
-- ---------------------------------------------------------------------------------
initialBeta::Int
initialBeta = (1000000)

-- ---------------------------------------------------------------------------------
-- Function: maximizeMoves
-- Description: Gets the maximun value for the computer's choice.
-- ---------------------------------------------------------------------------------
maximizeMoves::Board -> PColor -> Int -> Int -> Int -> (Board -> Int) ->
               TreeMoves -> Int -> Int -> Int -> Int
maximizeMoves board color level alpha beta evaluator treeMoves maxValue i total
    | (i > total) = maxValue
    | (alpha > beta) = maxValue
    | (maxValue < actValue) = maximizeMoves board color level actValue beta evaluator
                                            treeMoves actValue (i + 1) total
    | otherwise = maximizeMoves board color level alpha beta evaluator
                                treeMoves maxValue (i + 1) total
    where move = getMoveFromTree treeMoves i
          actValue = minimize (makeAIMove board move) (oppColor color) (level - 1)
                              alpha beta evaluator

-- ---------------------------------------------------------------------------------
-- Function: maximize
-- Description: Gets the maximun value for the computer's choice.
-- ---------------------------------------------------------------------------------
maximize::Board -> PColor -> Int -> Int -> Int -> (Board -> Int) -> Int
maximize board color 0 alpha beta evaluator = (evaluator board)
maximize board color level alpha beta evaluator = bestValue
    where treeMoves = getTreeMoves board color
          totalMoves = getTreeTotalMoves treeMoves
          bestValue = maximizeMoves board color level alpha beta evaluator
                                    treeMoves initialAlpha 1 totalMoves

-- ---------------------------------------------------------------------------------
-- Function: minimizeMoves
-- Description: Gets the minimun value for the player's choice.
-- ---------------------------------------------------------------------------------
minimizeMoves::Board -> PColor -> Int -> Int -> Int -> (Board -> Int) ->
               TreeMoves -> Int -> Int -> Int -> Int
minimizeMoves board color level alpha beta evaluator treeMoves minValue i total
    | (i > total) = minValue
    | (alpha > beta) = minValue
    | (minValue > actValue) = minimizeMoves board color level alpha actValue evaluator
                                            treeMoves actValue (i + 1) total
    | otherwise = minimizeMoves board color level alpha beta evaluator
                                treeMoves minValue (i + 1) total
    where move = getMoveFromTree treeMoves i
          actValue = maximize (makeAIMove board move) (oppColor color) (level - 1)
                              alpha beta evaluator

-- ---------------------------------------------------------------------------------
-- Function: minimize
-- Description: Gets the minimun value for the player's choice.
-- ---------------------------------------------------------------------------------
minimize::Board -> PColor -> Int -> Int -> Int -> (Board -> Int) -> Int
minimize board color 0 alpha beta evaluator = (evaluator board)
minimize board color level alpha beta evaluator = worstValue
    where treeMoves = getTreeMoves board color
          totalMoves = getTreeTotalMoves treeMoves
          worstValue = minimizeMoves board color level alpha beta evaluator
                                     treeMoves initialBeta 1 totalMoves

-- ---------------------------------------------------------------------------------
-- Function: minimaxMoves
-- Description: Gets the best move for the computer.
-- ---------------------------------------------------------------------------------
minimaxMoves::Board -> PColor -> Int -> Int -> Int -> (Board -> Int) ->
              TreeMoves -> Int -> Int -> Int -> [Position] -> [Position]
minimaxMoves board color level alpha beta evaluator treeMoves maxValue i total bestMove
    | (i > total) = bestMove
    | (maxValue < actValue) = minimaxMoves board color level actValue beta evaluator
                                           treeMoves actValue (i + 1) total move
    | otherwise = minimaxMoves board color level alpha beta evaluator
                               treeMoves maxValue (i + 1) total bestMove
    where move = getMoveFromTree treeMoves i
          actValue = minimize (makeAIMove board move) (oppColor color) (level - 1)
                              alpha beta evaluator

-- ---------------------------------------------------------------------------------
-- Function: minimax
-- Description: Gets the best move for the computer.
-- ---------------------------------------------------------------------------------
minimax::Board -> PColor -> Int -> Int -> Int -> [Position]
minimax board color level alpha beta = bestMove
    where treeMoves = getTreeMoves board color
          totalMoves = getTreeTotalMoves treeMoves
          evaluator = if color == Black then evalBoardWhite
                                        else evalBoardBlack
          bestMove = minimaxMoves board color level alpha beta evaluator
                                  treeMoves alpha 1 totalMoves (getMoveFromTree treeMoves 1)

-- ---------------------------------------------------------------------------------
-- Function: makeAIMove
-- Description: Executes a list of moves for the computer.
-- ---------------------------------------------------------------------------------
makeAIMove::Board -> [Position] -> Board
makeAIMove board [dest] = board
makeAIMove board (orig:(dest:xs)) = makeAIMove (makeMove board orig dest) (dest:xs)

-- ---------------------------------------------------------------------------------
-- Function: getLastMoveFromListLast
-- Description: Gets the last position inside the list.
-- ---------------------------------------------------------------------------------
getLastMoveFromListLast::[Position] -> Position -> Position
getLastMoveFromListLast [] last = last
getLastMoveFromListLast (x:xs) last = getLastMoveFromListLast xs x

-- ---------------------------------------------------------------------------------
-- Function: getLastMoveFromList
-- Description: Transforms a list of moves into a LastMove structure.
-- ---------------------------------------------------------------------------------
getLastMoveFromList::[Position] -> LastMove
getLastMoveFromList (x:xs) = (x, getLastMoveFromListLast xs x)

-- ---------------------------------------------------------------------------------
-- Function: aiMove
-- Description: Calculates and executes the best move for the computer.
-- ---------------------------------------------------------------------------------
aiMove::Board -> Config -> (Board, LastMove)
aiMove board (color, level)
    | (bestMove == []) = (board, ((1, 1), (1, 1)))
    | otherwise = (newBoard, lastMove)
    where numLevels = case level of
                           Easy -> 2
                           Medium -> 4
                           Hard -> 6
          bestMove = minimax board (oppColor color) numLevels initialAlpha initialBeta
          newBoard = makeAIMove board bestMove
          lastMove = getLastMoveFromList bestMove


-- *********************************************************************************
--                                       Move
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: execMove
-- Description: Executes a player's move inside the board.
-- ---------------------------------------------------------------------------------
execMove::Board -> PColor -> Position -> Position -> (Board, Int)
execMove board color orig dest
    | validMove && stillMove = (newBoard, 0)
    | validMove = (newBoard, 1)
    | otherwise = (board, -1)
    where validMove = validateMove board orig dest
          stillMove = stillKillMove board color orig dest
          newBoard = makeMove board orig dest

-- ---------------------------------------------------------------------------------
-- Function: moveEx
-- Description: Executes a player's move inside the board.
-- ---------------------------------------------------------------------------------
moveEx::GameData -> Position -> Position -> GameData
moveEx (board, last, cfg) orig dest
    | (error == 0) = (newBoard1, ((0, 0), dest), cfg)
    | (error == 1) = (newBoard2, aiLastMove, cfg)
    | otherwise = (board, ((-1, -1), (0, 0)), cfg)
    where (color, level) = cfg
          (newBoard1, error) = execMove board color orig dest
          (newBoard2, aiLastMove) = aiMove newBoard1 cfg

-- ---------------------------------------------------------------------------------
-- Function: move
-- Description: Executes a player's move inside the board.
-- ---------------------------------------------------------------------------------
move::GameData -> Position -> Position -> GameData
move (board, ((0, 0), (r, c)), cfg) orig dest
    | validPiece = moveEx (board, ((0, 0), (r, c)), cfg) orig dest
    | otherwise = (board, ((-1, -3), (0, 0)), cfg)
    where validPiece = (orig == (r, c))
move (board, last, cfg) orig dest
    | validPiece = moveEx (board, last, cfg) orig dest
    | otherwise = (board, ((-1, -2), (0, 0)), cfg)
    where (color, level) = cfg
          validPiece = (validateOrigin board color orig)


-- **************************************************************************************
-- * End Logic.hs                                                                       *
-- **************************************************************************************