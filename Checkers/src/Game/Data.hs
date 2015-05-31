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
module Game.Data where

-- **************************************************************************************
-- * * * * * * * * * * * * * * * * * * * * Piece  * * * * * * * * * * * * * * * * * * * *
-- **************************************************************************************

-- *********************************************************************************
--                                       Types
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Type name: PColor
-- Description: This new type is to define the color of a piece.
-- ---------------------------------------------------------------------------------
data PColor = White | Black
     deriving (Eq, Enum, Show, Read)

-- ---------------------------------------------------------------------------------
-- Type name: PType
-- Description: This new type is to define the type of a piece: normal or king.
--              The main diference between the types, is the move options.
-- ---------------------------------------------------------------------------------
data PType = Normal | King
     deriving (Eq, Enum, Show, Read)

-- ---------------------------------------------------------------------------------
-- Type name: Piece
-- Description: This represents the data of a piece: the color and the type.
-- ---------------------------------------------------------------------------------
type Piece = (PColor, PType)



-- **************************************************************************************
-- * * * * * * * * * * * * * * * * * * * * Board  * * * * * * * * * * * * * * * * * * * *
-- **************************************************************************************

-- *********************************************************************************
--                                       Types
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Type name: Square
-- Description: This new type is to define the state of a square in the board of
--              the game. There is only three options: an empty square, a square
--              with a piece or a square that is not usable in the game.
-- ---------------------------------------------------------------------------------
data Square = SquareEmpty | SquarePiece Piece | SquareInvalid
     deriving (Eq, Show, Read)

-- ---------------------------------------------------------------------------------
-- Type name: Position
-- Description: This represents a position inside the board.
-- ---------------------------------------------------------------------------------
type Position = (Int, Int)

-- ---------------------------------------------------------------------------------
-- Type name: BoardRow
-- Description: This represents a file in the board of the game.
-- ---------------------------------------------------------------------------------
type BoardRow = [Square]

-- ---------------------------------------------------------------------------------
-- Type name: Board
-- Description: This represents the board of the game.
-- ---------------------------------------------------------------------------------
type Board = [BoardRow]


-- *********************************************************************************
--                                     Functions
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: initBoardRow
-- Description: This gives an initialized BoardRow.
-- ---------------------------------------------------------------------------------
initBoardRow::Square -> Int -> BoardRow
initBoardRow square n = [square | x <- [1..n]]

-- ---------------------------------------------------------------------------------
-- Function: initBoardSquares
-- Description: This gives an initialized fragment of a Board.
-- ---------------------------------------------------------------------------------
initBoardSquares::Square -> Int -> Board
initBoardSquares square n = [initBoardRow square 4 | x <- [1..n]]

-- ---------------------------------------------------------------------------------
-- Function: initBoard
-- Description: This gives an initialized Board.
-- ---------------------------------------------------------------------------------
initBoard::Board
initBoard = initBoardSquares (SquarePiece (White, Normal)) 3 ++
            initBoardSquares SquareEmpty 2 ++
            initBoardSquares (SquarePiece (Black, Normal)) 3


-- ---------------------------------------------------------------------------------
-- Function: checkLimits
-- Description: Checks if the coordinates are inside the board.
-- ---------------------------------------------------------------------------------
checkLimits::Int -> Int -> Bool
checkLimits row col = (1 <= row) && (row <= 8) && (1 <= col) && (col <= 8)

-- ---------------------------------------------------------------------------------
-- Function: checkEvenCoords
-- Description: Checks if the coordinates are an even number.
-- ---------------------------------------------------------------------------------
checkEvenCoords::Int -> Int -> Bool
checkEvenCoords row col = (even row) && (even col)

-- ---------------------------------------------------------------------------------
-- Function: checkOddCoords
-- Description: Checks if the coordinates are an odd number.
-- ---------------------------------------------------------------------------------
checkOddCoords::Int -> Int -> Bool
checkOddCoords row col = (odd row) && (odd col)

-- ---------------------------------------------------------------------------------
-- Function: checkEvenLimits
-- Description: Checks if the coordinates are an even square.
-- ---------------------------------------------------------------------------------
checkEvenLimits::Int -> Int -> Bool
checkEvenLimits row col = (checkLimits row col) && (checkEvenCoords row col)

-- ---------------------------------------------------------------------------------
-- Function: checkOddLimits
-- Description: Checks if the coordinates are an odd square.
-- ---------------------------------------------------------------------------------
checkOddLimits::Int -> Int -> Bool
checkOddLimits row col = (checkLimits row col) && (checkOddCoords row col)


-- ---------------------------------------------------------------------------------
-- Function: getBoardCol
-- Description: Gets a square inside a BoardRow value.
-- ---------------------------------------------------------------------------------
getBoardCol::BoardRow -> Int -> Square
getBoardCol [] col = SquareInvalid
getBoardCol (x:xs) 1 = x
getBoardCol (x:xs) col = getBoardCol xs (col-1)

-- ---------------------------------------------------------------------------------
-- Function: getBoardRow
-- Description: Gets a row inside a Board value.
-- ---------------------------------------------------------------------------------
getBoardRow::Board -> Int -> BoardRow
getBoardRow [] row = []
getBoardRow (x:xs) 1 = x
getBoardRow (x:xs) row = getBoardRow xs (row-1)

-- ---------------------------------------------------------------------------------
-- Function: getBoardSquare
-- Description: Gets a square inside a Board value.
-- ---------------------------------------------------------------------------------
getBoardSquare::Board -> Int -> Int -> Square
getBoardSquare xs row col
    | (checkEvenLimits row col) = getBoardCol (getBoardRow xs row) (div col 2)
    | (checkOddLimits row col) = getBoardCol (getBoardRow xs row) (div (col + 1) 2)
    | otherwise = SquareInvalid

-- ---------------------------------------------------------------------------------
-- Function: setBoardCol
-- Description: Sets a square inside a BoardRow value.
-- ---------------------------------------------------------------------------------
setBoardCol::BoardRow -> Int -> Square -> BoardRow
setBoardCol [] col value = []
setBoardCol (x:xs) 1 value = value : xs
setBoardCol (x:xs) col value = x : (setBoardCol xs (col-1) value)

-- ---------------------------------------------------------------------------------
-- Function: setBoardRow
-- Description: Sets a square inside a Board value.
-- ---------------------------------------------------------------------------------
setBoardRow::Board -> Int -> Int -> Square -> Board
setBoardRow [] row col value = []
setBoardRow (x:xs) 1 col value = (setBoardCol x col value) : xs
setBoardRow (x:xs) row col value = x : (setBoardRow xs (row-1) col value)

-- ---------------------------------------------------------------------------------
-- Function: setBoardSquare
-- Description: Sets a square inside a Board value.
-- ---------------------------------------------------------------------------------
setBoardSquare::Board -> Int -> Int -> Square -> Board
setBoardSquare xs row col value
    | (checkEvenLimits row col) = (setBoardRow xs row (div col 2) value)
    | (checkOddLimits row col) = (setBoardRow xs row (div (col + 1) 2) value)
    | otherwise = xs



-- **************************************************************************************
-- * * * * * * * * * * * * * * * * * * *  GameData  * * * * * * * * * * * * * * * * * * *
-- **************************************************************************************

-- *********************************************************************************
--                                       Types
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Type name: Level
-- Description: This new type is to define the difficulty in the game.
-- ---------------------------------------------------------------------------------
data Level = Easy | Medium | Hard
     deriving (Eq, Enum, Show, Read)

-- ---------------------------------------------------------------------------------
-- Type name: Config
-- Description: This represents the current configuration of the game.
-- ---------------------------------------------------------------------------------
type Config = (PColor, Level)

-- ---------------------------------------------------------------------------------
-- Type name: LastMove
-- Description: This represents the las move of the IA.
-- ---------------------------------------------------------------------------------
type LastMove = (Position, Position)

-- ---------------------------------------------------------------------------------
-- Type name: GameData
-- Description: This represents the current state of the whole game.
-- ---------------------------------------------------------------------------------
type GameData = (Board, LastMove, Config)


-- *********************************************************************************
--                                     Functions
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: initLastMove
-- Description: This gives an initialized LastMove.
-- ---------------------------------------------------------------------------------
initLastMove::LastMove
initLastMove = ((1, 1), (1, 1))

-- ---------------------------------------------------------------------------------
-- Function: initConfig
-- Description: This gives an initialized Config.
-- ---------------------------------------------------------------------------------
initConfig::Config
initConfig = (White, Easy)

-- ---------------------------------------------------------------------------------
-- Function: initGameData
-- Description: This gives an initialized GameData.
-- ---------------------------------------------------------------------------------
initGameData::GameData
initGameData = (initBoard, initLastMove, initConfig)


-- **************************************************************************************
-- * End Data.hs                                                                        *
-- **************************************************************************************