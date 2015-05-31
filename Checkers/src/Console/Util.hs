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
module Console.Util where

-- ---------------------------------------------------------------------------------
-- Function: dropTokenEx
-- Description: Drops the first token inside the string.
-- ---------------------------------------------------------------------------------
dropTokenEx::String -> Int -> String
dropTokenEx [] n = []
dropTokenEx text 0 = text
dropTokenEx text n = dropTokenEx nextText (n-1)
    where nextText = drop 1 (dropWhile (' ' /=) text)

-- ---------------------------------------------------------------------------------
-- Function: dropToken
-- Description: Drops the first token inside the string.
-- ---------------------------------------------------------------------------------
dropToken::String -> String
dropToken text = dropTokenEx text 1

-- ---------------------------------------------------------------------------------
-- Function: getToken
-- Description: Gets a token inside the string.
-- ---------------------------------------------------------------------------------
getToken::String -> Int -> String
getToken [] n = []
getToken text 1 = takeWhile (' ' /=) text
getToken text n = getToken nextText (n-1)
    where nextText = drop 1 (dropWhile (' ' /=) text)

-- ---------------------------------------------------------------------------------
-- Function: getTokens
-- Description: Gets a some tokens inside the string.
-- ---------------------------------------------------------------------------------
getTokens::String -> Int -> String
getTokens [] n = []
getTokens text 1 = getToken text 1
getTokens text n
    | (token /= "") = (getTokens text (n-1)) ++ " " ++ token
    | otherwise = (getTokens text (n-1))
    where token = (getToken text n)

-- ---------------------------------------------------------------------------------
-- Function: countTokensEx
-- Description: Gets the number of tokens inside the string.
-- ---------------------------------------------------------------------------------
countTokensEx::String -> Int -> Int
countTokensEx text i
    | ((getToken text (i+1)) /= "") = (countTokensEx text (i+1))
    | otherwise = i

-- ---------------------------------------------------------------------------------
-- Function: countTokens
-- Description: Gets the number of tokens inside the string.
-- ---------------------------------------------------------------------------------
countTokens::String -> Int
countTokens text = countTokensEx text 0

-- ---------------------------------------------------------------------------------
-- Function: getTokenPositionEx
-- Description: Gets the position of a token inside the text.
-- ---------------------------------------------------------------------------------
getTokenPositionEx::String -> String -> Int -> Int
getTokenPositionEx [] elem n = 0
getTokenPositionEx text elem n
    | ((getToken text 1) == elem) = n
    | otherwise = getTokenPositionEx (dropToken text) elem (n+1)

-- ---------------------------------------------------------------------------------
-- Function: getTokenPosition
-- Description: Gets the position of a token inside the text.
-- ---------------------------------------------------------------------------------
getTokenPosition::String -> String -> Int
getTokenPosition text elem = getTokenPositionEx text elem 1

-- ---------------------------------------------------------------------------------
-- Function: checkText
-- Description: Checks if some tokens inside "text" are equal to "cmd".
-- ---------------------------------------------------------------------------------
checkText::String -> String -> Int -> Bool
checkText text cmd n = (line == cmd)
    where line = getTokens text n

-- ---------------------------------------------------------------------------------
-- Function: isDigit
-- Description: Checks if the character is a digit.
-- ---------------------------------------------------------------------------------
isDigit:: Char -> Bool
isDigit c = or [c == x | x <- "0123456789"]

-- ---------------------------------------------------------------------------------
-- Function: quitSpacesEx
-- Description: Erases the redundant blank spaces in a string.
-- ---------------------------------------------------------------------------------
quitSpacesEx::String -> Bool -> String
quitSpacesEx [] p = []
quitSpacesEx (' ':cs) True = (quitSpacesEx cs True)
quitSpacesEx (' ':cs) False = ' ' : (quitSpacesEx cs True)
quitSpacesEx (c:cs) p = c : (quitSpacesEx cs False)

-- ---------------------------------------------------------------------------------
-- Function: quitSpaces
-- Description: Erases the redundant blank spaces in a string.
-- ---------------------------------------------------------------------------------
quitSpaces::String -> String
quitSpaces cs = quitSpacesEx cs True

-- **************************************************************************************
-- * End Util.hs                                                                        *
-- **************************************************************************************