-- **************************************************************************************
-- * Checkers 1.0 - The spanish checkers game implemented in Haskell.                   *
-- * Copyright (C) 2007  Gorka Su�rez Garc�a & Enrique L�pez Ma�as                      *
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
module Checkers (main) where

-- **************************************************************************************
-- * External modules                                                                   *
-- **************************************************************************************
import Game.Data
import Game.Logic
import Console.Console
import Console.Util

-- **************************************************************************************
-- * Function: main                                                                     *
-- * Description: This is the main entry to the game.                                   *
-- **************************************************************************************
main = do putStrLn "Welcome to the checkers game!"
          putStrLn "If you need help type the 'help' command, please.\n"
          doCmdLookBoard kernel
          mainLoop kernel
    where kernel = initKernel

-- **************************************************************************************
-- * End Checkers.hs                                                                    *
-- **************************************************************************************