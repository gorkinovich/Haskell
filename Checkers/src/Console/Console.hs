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
module Console.Console where

-- **************************************************************************************
-- * External modules                                                                   *
-- **************************************************************************************
import Data.Char (toLower)
import Game.Data
import Game.Logic
import Console.Util

-- **************************************************************************************
-- * * * * * * * * * * * * * * * * * * * * Kernel * * * * * * * * * * * * * * * * * * * *
-- **************************************************************************************

-- *********************************************************************************
--                                       Types
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Type name: Command
-- Description: This new type is to define the executable commands in the game.
-- ---------------------------------------------------------------------------------
data Command = CmdHelp | CmdLookBoard | CmdNewGame PColor Level |
               CmdRestart | CmdExit | CmdMove Position Position |
               CmdNone
     deriving (Eq, Show, Read)

-- ---------------------------------------------------------------------------------
-- Type name: Kernel
-- Description: This represents the current state of the whole application.
-- ---------------------------------------------------------------------------------
type Kernel = (GameData, Command)


-- *********************************************************************************
--                                     Functions
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: initKernel
-- Description: This gives an initialized Kernel.
-- ---------------------------------------------------------------------------------
initKernel::Kernel
initKernel = (initGameData, CmdNone)



-- **************************************************************************************
-- * * * * * * * * * * * * * * * * * * * * Console  * * * * * * * * * * * * * * * * * * *
-- **************************************************************************************

-- *********************************************************************************
--                                   Check Command
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: checkCommandEx
-- Description: Checks if the line is a certain command, and if it's
--              true the function will return the command inside value.
-- ---------------------------------------------------------------------------------
checkCommandEx::String -> String -> Int -> Command -> Command
checkCommandEx text cmd n value
    | (checkText text cmd n) = value
    | otherwise = CmdNone

-- ---------------------------------------------------------------------------------
-- Function: checkCommand
-- Description: Checks if the line is a certain command, and if it's
--              true the function will return the command inside value.
-- ---------------------------------------------------------------------------------
checkCommand::String -> String -> Command -> Command
checkCommand text cmd value
    | (text == cmd) = value
    | otherwise = CmdNone

-- ---------------------------------------------------------------------------------
-- Function: parsePieceColor
-- Description: Gets a PieceColor from a string.
-- ---------------------------------------------------------------------------------
parsePieceColor::String -> PColor
parsePieceColor "whites" = White
parsePieceColor "blacks" = Black
parsePieceColor x = White

-- ---------------------------------------------------------------------------------
-- Function: parseLevel
-- Description: Gets a Level from a string.
-- ---------------------------------------------------------------------------------
parseLevel::String -> Level
parseLevel "easy" = Easy
parseLevel "medium" = Medium
parseLevel "hard" = Hard
parseLevel x = Easy

-- ---------------------------------------------------------------------------------
-- Function: getStrPieceColor
-- Description: Get a string from a PieceColor.
-- ---------------------------------------------------------------------------------
getStrPieceColor::PColor -> String
getStrPieceColor White = "whites"
getStrPieceColor Black = "blacks"

-- ---------------------------------------------------------------------------------
-- Function: getStrLevel
-- Description: Get a string from a Difficulty.
-- ---------------------------------------------------------------------------------
getStrLevel::Level -> String
getStrLevel Easy = "easy"
getStrLevel Medium = "medium"
getStrLevel Hard = "hard"

-- ---------------------------------------------------------------------------------
-- Function: parsePositionEx
-- Description: Gets a BoardPosition from a string.
-- ---------------------------------------------------------------------------------
parsePositionEx::String -> String -> Position -> Position
parsePositionEx (c:cs) lastNum (row, col)
    | (c == ')') = (row, (read lastNum))
    | (c == ',') = parsePositionEx cs "" ((read lastNum), col)
    | (c == ' ') = parsePositionEx cs lastNum (row, col)
    | isDigit c = parsePositionEx cs (lastNum++[c]) (row, col)
    | otherwise = (0, 0)

-- ---------------------------------------------------------------------------------
-- Function: parsePosition
-- Description: Gets a BoardPosition from a string.
-- ---------------------------------------------------------------------------------
parsePosition::String -> Position
parsePosition (c:cs)
    | (c == '(') = parsePositionEx cs "" (0, 0)
    | otherwise = (0, 0)


-- *********************************************************************************
--                                 Check Command New
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: getStrCmdNew3
-- Description: Get a string like "new <level> game" from a Command.
-- ---------------------------------------------------------------------------------
getStrCmdNew3::Command -> String
getStrCmdNew3 (CmdNewGame color level) = "new " ++ (getStrLevel level) ++ " game"

-- ---------------------------------------------------------------------------------
-- Function: getStrCmdNew4
-- Description: Get a string like "new game with <color>" from a Command.
-- ---------------------------------------------------------------------------------
getStrCmdNew4::Command -> String
getStrCmdNew4 (CmdNewGame color level) = "new game with " ++ (getStrPieceColor color)

-- ---------------------------------------------------------------------------------
-- Function: getStrCmdNew5
-- Description: Get a string like "new <level> game with <color>" from a Command.
-- ---------------------------------------------------------------------------------
getStrCmdNew5::Command -> String
getStrCmdNew5 (CmdNewGame color level) = "new " ++ (getStrLevel level) ++
                                         " game with " ++ (getStrPieceColor color)

-- ---------------------------------------------------------------------------------
-- Function: checkCmdNewEx3
-- Description: Checks if the line is a new game command, and if it's
--              true the function will return the command inside value.
-- ---------------------------------------------------------------------------------
checkCmdNewEx3::String -> Command
checkCmdNewEx3 text = checkCommand text lineCmd cmd
    where cmd = (CmdNewGame White (parseLevel (getToken text 2)))
          lineCmd = (getStrCmdNew3 cmd)

-- ---------------------------------------------------------------------------------
-- Function: checkCmdNewEx4
-- Description: Checks if the line is a new game command, and if it's
--              true the function will return the command inside value.
-- ---------------------------------------------------------------------------------
checkCmdNewEx4::String -> Command
checkCmdNewEx4 text = checkCommand text lineCmd cmd
    where cmd = (CmdNewGame (parsePieceColor (getToken text 4)) Easy)
          lineCmd = (getStrCmdNew4 cmd)

-- ---------------------------------------------------------------------------------
-- Function: checkCmdNewEx5
-- Description: Checks if the line is a new game command, and if it's
--              true the function will return the command inside value.
-- ---------------------------------------------------------------------------------
checkCmdNewEx5::String -> Command
checkCmdNewEx5 text = checkCommand text lineCmd cmd
    where cmd = (CmdNewGame (parsePieceColor (getToken text 5)) (parseLevel (getToken text 2)))
          lineCmd = (getStrCmdNew5 cmd)

-- ---------------------------------------------------------------------------------
-- Function: checkCmdNew
-- Description: Checks if the line is a new game command, and if it's
--              true the function will return the command inside value.
-- ---------------------------------------------------------------------------------
checkCmdNew::String -> Command
checkCmdNew text
    | (tokens == 2) = checkCommandEx text "new game" 2 (CmdNewGame White Easy)
    | (tokens == 3) = checkCmdNewEx3 text
    | (tokens == 4) = checkCmdNewEx4 text
    | (tokens == 5) = checkCmdNewEx5 text
    | otherwise = CmdNone
	where tokens = (countTokens text)


-- *********************************************************************************
--                                Check Command Move
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: checkCmdMovEx
-- Description: Checks if the line is a move command, and if it's
--              true the function will return the command inside value.
-- ---------------------------------------------------------------------------------
checkCmdMovEx::String -> Int -> Command
checkCmdMovEx text tokens
    | (1 < toPos) && (toPos < tokens) = CmdMove
      (parsePosition (getTokens text (toPos-1)))
      (parsePosition (getTokens (dropTokenEx text toPos) (tokens-toPos)))
    | otherwise = CmdNone
    where toPos = getTokenPosition text "to"

-- ---------------------------------------------------------------------------------
-- Function: checkCmdMov
-- Description: Checks if the line is a move command, and if it's
--              true the function will return the command inside value.
-- ---------------------------------------------------------------------------------
checkCmdMov::String -> Command
checkCmdMov text
    | cond = checkCmdMovEx (dropToken line) (tokens-1)
    | otherwise = CmdNone
	where line = (quitSpaces text)
	      tokens = (countTokens line)
	      cond = ((getToken line 1) == "move") && (tokens > 3)


-- *********************************************************************************
--                                    Get Command
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: parseCmd
-- Description: Gets a Command from a string.
-- ---------------------------------------------------------------------------------
parseCmd::String -> Command
parseCmd text = case (head line) of
                     'h' -> checkCommandEx line "help" 1 CmdHelp
                     'l' -> checkCommandEx line "look board" 2 CmdLookBoard
                     'n' -> checkCmdNew line
                     'r' -> checkCommandEx line "restart" 1 CmdRestart
                     'm' -> checkCmdMov line
                     'e' -> checkCommandEx line "exit" 1 CmdExit
                     otherwise -> CmdNone
    where line = (map toLower text)

-- ---------------------------------------------------------------------------------
-- Function: getCmd
-- Description: Gets a Command from a string.
-- ---------------------------------------------------------------------------------
getCmd::Kernel -> IO Kernel
getCmd (game, _) = do putStr "> "
                      cmd <- getLine
                      return (game, (parseCmd cmd))


-- *********************************************************************************
--                                    Do Command
-- *********************************************************************************

-- ---------------------------------------------------------------------------------
-- Function: doCmdHelp
-- Description: Shows the help of the game.
-- ---------------------------------------------------------------------------------
doCmdHelp::Kernel -> IO Kernel
doCmdHelp kernel = do putStrLn "This are the valid commands in the game:\n"
                      putStrLn "Help                                Shows this help."
                      putStrLn "Look board                          Shows the current board."
                      putStrLn "New [<level>] game [with <color>]   Starts a new game."
                      putStrLn "+ <level> = easy, medium, hard      The level of the game."
                      putStrLn "+ <color> = whites, blacks          The color of the player."
                      putStrLn "Restart                             Restarts the game."
                      putStrLn "Move (row, col) to (row2, col2)     Moves a piece."
                      putStrLn "Exit                                Exits this game.\n"
                      putStrLn "The [] in the new game command means a part from the command wich is optional."
                      putStrLn "Some examples of the new game command:"
                      putStrLn "New game                      A new game with easy level and white color."
                      putStrLn "New hard game                 A new game with hard level and white color."
                      putStrLn "New game with blacks          A new game with easy level and black color."
                      putStrLn "New medium game with whites   A new game with medium level and white color.\n"
                      putStrLn "And finally the move to command, where the first coordinates are the piece you"
                      putStrLn "want to select to move, and the second coordinates are the square you wan to"
                      putStrLn "move the piece you have chosen. An example: move (3, 1) to (4, 2)\n"
                      return kernel

-- ---------------------------------------------------------------------------------
-- Function: printSquare
-- Description: Prints the value of a square on screen.
-- ---------------------------------------------------------------------------------
printSquare::Square -> IO ()
printSquare SquareInvalid = putStr "| . "
printSquare SquareEmpty = putStr "|   "
printSquare (SquarePiece (White, Normal)) = putStr "| w "
printSquare (SquarePiece (White, King)) = putStr "| W "
printSquare (SquarePiece (Black, Normal)) = putStr "| b "
printSquare (SquarePiece (Black, King)) = putStr "| B "

-- ---------------------------------------------------------------------------------
-- Function: printHBorderLn
-- Description: Prints the upper and bottom horizontal border.
-- ---------------------------------------------------------------------------------
printHBorderLn::IO ()
printHBorderLn = putStr "   ---------------------------------\n"

-- ---------------------------------------------------------------------------------
-- Function: printHNumbersLn
-- Description: Prints the horizontal numbers of the columns.
-- ---------------------------------------------------------------------------------
printHNumbersLn::IO ()
printHNumbersLn = putStr "     1   2   3   4   5   6   7   8\n"

-- ---------------------------------------------------------------------------------
-- Function: printHSeparator
-- Description: Prints the horizontal separator between squares.
-- ---------------------------------------------------------------------------------
printHSeparator::IO ()
printHSeparator = putStr "   | - + - + - + - + - + - + - + - |   "

-- ---------------------------------------------------------------------------------
-- Function: printHSeparatorLn
-- Description: Prints the horizontal separator between squares.
-- ---------------------------------------------------------------------------------
printHSeparatorLn::IO ()
printHSeparatorLn = do printHSeparator
                       putStrLn ""

-- ---------------------------------------------------------------------------------
-- Function: printBoardCols
-- Description: Prints the columns of a row.
-- ---------------------------------------------------------------------------------
printBoardCols::Board -> Int -> Int -> IO ()
printBoardCols board row 8 = printSquare (getBoardSquare board row 8)
printBoardCols board row col = do printSquare (getBoardSquare board row col)
                                  printBoardCols board row (col+1)

-- ---------------------------------------------------------------------------------
-- Function: printBoardRow
-- Description: Prints a row of squares.
-- ---------------------------------------------------------------------------------
printBoardRow::Board -> Int -> IO ()
printBoardRow board row = do putStr strRow
                             printBoardCols board row 1
                             putStr ('|' : strRow)
    where strRow = ' ' : (show row ++ " ")

-- ---------------------------------------------------------------------------------
-- Function: printBoardRowLn
-- Description: Prints a row of squares.
-- ---------------------------------------------------------------------------------
printBoardRowLn::Board -> Int -> IO ()
printBoardRowLn board row = do printBoardRow board row
                               putStrLn ""

-- ---------------------------------------------------------------------------------
-- Function: printBoardRows
-- Description: Prints some rows of the board.
-- ---------------------------------------------------------------------------------
printBoardRows::Board -> Int -> IO ()
printBoardRows board 1 = do printHSeparatorLn
                            printBoardRowLn board 1
printBoardRows board row = do printHSeparatorLn
                              printBoardRowLn board row
                              printBoardRows board (row-1)

-- ---------------------------------------------------------------------------------
-- Function: doCmdLookBoard
-- Description: Shows on screen the current board of the game.
-- ---------------------------------------------------------------------------------
doCmdLookBoard::Kernel -> IO Kernel
doCmdLookBoard kernel = do printHNumbersLn
                           printHBorderLn
                           printBoardRow board 8
                           putStrLn "   Symbols reference"
                           printHSeparator
                           putStrLn "   w = White piece"
                           printBoardRow board 7
                           putStrLn "   b = Black piece"
                           printHSeparator
                           putStrLn "   W = White king piece"
                           printBoardRow board 6
                           putStrLn "   B = Black king piece"
                           printBoardRows board 5
                           printHBorderLn
                           printHNumbersLn
                           putStrLn ""
                           return kernel
    where ((board, _, _), _) = kernel

--   "     1   2   3   4   5   6   7   8                               "
--   "   ---------------------------------                             "
--   " 8 | . | b | . | b | . | b | . | b | 8    Symbols reference      "
--   "   | - + - + - + - + - + - + - + - |      w = White piece        "
--   " 7 | b | . | b | . | b | . | b | . | 7    b = Black piece        "
--   "   | - + - + - + - + - + - + - + - |      W = White king piece   "
--   " 6 | . | b | . | b | . | b | . | b | 6    B = Black king piece   "
--   "   | - + - + - + - + - + - + - + - |                             "
--   " 5 |   | . |   | . |   | . |   | . | 5                           "
--   "   | - + - + - + - + - + - + - + - |                             "
--   " 4 | . |   | . |   | . |   | . |   | 4                           "
--   "   | - + - + - + - + - + - + - + - |                             "
--   " 3 | w | . | w | . | w | . | w | . | 3                           "
--   "   | - + - + - + - + - + - + - + - |                             "
--   " 2 | . | w | . | w | . | w | . | w | 2                           "
--   "   | - + - + - + - + - + - + - + - |                             "
--   " 1 | w | . | w | . | w | . | w | . | 1                           "
--   "   ---------------------------------                             "
--   "     1   2   3   4   5   6   7   8                               "


-- ---------------------------------------------------------------------------------
-- Function: doCmdNewGame
-- Description: Starts a new game with a new configuration.
-- ---------------------------------------------------------------------------------
doCmdNewGame::Kernel -> IO Kernel
doCmdNewGame (_, (CmdNewGame color level)) =
    do putStrLn "A new game have been started."
       return kernel
    where newCfg = (color, level)
          kernel = ((initBoard, initLastMove, newCfg), (CmdNewGame color level))
doCmdNewGame x = do putStrLn "Invalid command to create a new game."
                    return x

-- ---------------------------------------------------------------------------------
-- Function: doCmdRestart
-- Description: Starts a new game with the current configuration.
-- ---------------------------------------------------------------------------------
doCmdRestart::Kernel -> IO Kernel
doCmdRestart ((_, _, cfg), CmdRestart) =
    do putStrLn "The game have been restarted."
       return kernel
    where kernel = ((initBoard, initLastMove, cfg), CmdRestart)
doCmdRestart x = do putStrLn "Invalid command to restart the game."
                    return x

-- ---------------------------------------------------------------------------------
-- Function: getMoveError
-- Description: Gets the string that explains the last error in a move.
-- ---------------------------------------------------------------------------------
getMoveError::Int -> String
getMoveError (-1) = "That move is invalid!!!"
getMoveError (-2) = "That piece is invalid!!!"
getMoveError (-3) = "Please keep moving your last piece!!!"
getMoveError x = "Unknown error!!!"

-- ---------------------------------------------------------------------------------
-- Function: doCmdMove
-- Description: Executes a player's move inside the board.
-- ---------------------------------------------------------------------------------
doCmdMove::Kernel -> IO Kernel
doCmdMove (gameData, (CmdMove orig dest))
    | (orow == -1) = do putStrLn (getMoveError ocol)
                        return (gameData, (CmdMove orig dest))
    | (orow == 0) = do doCmdLookBoard kernel
                       putStrLn "The player still move."
                       return kernel
    | noAIMove = do doCmdLookBoard kernel
                    putStrLn ("The computer can't move.")
                    return kernel
    | otherwise = do doCmdLookBoard kernel
                     putStrLn ("The computer have moved ("++(show orow)++", "++(show ocol)++
                               ") to ("++(show drow)++", "++(show dcol)++").")
                     return kernel
    where (board, last, cfg) = move gameData orig dest
          ((orow, ocol), (drow, dcol)) = last
          noAIMove = (((orow, ocol), (drow, dcol)) == ((1, 1), (1, 1)))
          kernel = ((board, last, cfg), (CmdMove orig dest))
doCmdMove x = do putStrLn "Invalid command to move a piece."
                 return x

-- ---------------------------------------------------------------------------------
-- Function: doCmdError
-- Description: Shows an error if the user puts an invalid command.
-- ---------------------------------------------------------------------------------
doCmdError::Kernel -> IO Kernel
doCmdError kernel = do putStrLn "Invalid command, type help if you need a list of them."
                       return kernel

-- ---------------------------------------------------------------------------------
-- Function: doCmdEx
-- Description: Executes the last command introduced.
-- ---------------------------------------------------------------------------------
doCmdEx::Kernel -> Command -> IO Kernel
doCmdEx kernel (CmdHelp) = doCmdHelp kernel
doCmdEx kernel (CmdLookBoard) = doCmdLookBoard kernel
doCmdEx kernel (CmdNewGame _ _) = doCmdNewGame kernel
doCmdEx kernel (CmdRestart) = doCmdRestart kernel
doCmdEx kernel (CmdMove _ _) = doCmdMove kernel
doCmdEx kernel (CmdExit) = return kernel
doCmdEx kernel x = doCmdError kernel

-- ---------------------------------------------------------------------------------
-- Function: doCmd
-- Description: Executes the last command introduced.
-- ---------------------------------------------------------------------------------
doCmd::Kernel -> IO Kernel
doCmd kernel = doCmdEx kernel cmd
    where (_, cmd) = kernel


-- *********************************************************************************
--                                     Main loop
-- *********************************************************************************
-- ---------------------------------------------------------------------------------
-- Function: mainLoop
-- Description: This is the main loop of the game.
-- ---------------------------------------------------------------------------------
mainLoop::Kernel -> IO ()
mainLoop (game, cmd)
    | (cmd == CmdExit) = return ()
    | otherwise = do auxKernel1 <- getCmd (game, cmd)
                     auxKernel2 <- doCmd auxKernel1
                     mainLoop auxKernel2


-- **************************************************************************************
-- * End Console.hs                                                                     *
-- **************************************************************************************