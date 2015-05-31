-- **********************************************************************
-- Checkers 1.0 - The spanish checkers game implemented in Haskell.
-- Copyright (C) 2007  Gorka Suárez García & Enrique López Mañas
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
-- **********************************************************************
module Debug where

-- **********************************************************************
-- External modules
-- **********************************************************************
import Game.Data

-- **********************************************************************
-- Function: debugPiece
-- Description: This function shows the content of a Piece value.
-- **********************************************************************
debugPiece::Piece -> IO ()
debugPiece (c, t) = putStr ("("++(show c)++", "++(show t)++")")

-- **********************************************************************
-- Function: debugSquare
-- Description: This function shows the content of a Square value.
-- **********************************************************************
debugSquare::Square -> IO ()
debugSquare SquareEmpty = putStr "Empty"
debugSquare (SquarePiece piece) = do putStr "Piece "
                                     debugPiece piece
debugSquare SquareInvalid = putStr "Invalid"

-- **********************************************************************
-- Function: debugPosition
-- Description: This function shows the content of a Position value.
-- **********************************************************************
debugPosition::Position -> IO ()
debugPosition (r, c) = putStr ("("++(show r)++", "++(show c)++")")

-- **********************************************************************
-- Function: debugBoardRow
-- Description: This function shows the content of a BoardRow value.
-- **********************************************************************
debugBoardRow::BoardRow -> IO ()
debugBoardRow [x] = debugSquare x
debugBoardRow (x:xs) = do debugSquare x
                          putStr ", "
                          debugBoardRow xs

-- **********************************************************************
-- Function: debugBoard
-- Description: This function shows the content of a Board value.
-- **********************************************************************
debugBoard::Board -> IO ()
debugBoard [x] = do putStr "["
                    debugBoardRow x
                    putStr "]"
debugBoard (x:xs) = do putStr "["
                       debugBoardRow x
                       putStr "]\n\n"
                       debugBoard xs

-- **********************************************************************
-- Function: debugConfig
-- Description: This function shows the content of a Config value.
-- **********************************************************************
debugConfig::Config -> IO ()
debugConfig (c, l) = putStr ("("++(show c)++", "++(show l)++")")

-- **********************************************************************
-- Function: debugLastMove
-- Description: This function shows the content of a LastMove value.
-- **********************************************************************
debugLastMove::LastMove -> IO ()
debugLastMove (orig, dest) = do debugPosition orig
                                putStr " -> "
                                debugPosition dest

-- **********************************************************************
-- Function: debugGameData
-- Description: This function shows the content of a Kernel value.
-- **********************************************************************
debugGameData::GameData -> IO ()
debugGameData (board, move, cfg) =
    do putStrLn "Game Data\n====================\n"
       putStr "Board\n--------------------\n["
       debugBoard board
       putStrLn "]\n\nLast Move\n--------------------"
       debugLastMove move
       putStrLn "\n\nConfiguration\n--------------------"
       debugConfig cfg
       putStrLn "\n"

-- **********************************************************************
-- End Debug.hs
-- **********************************************************************