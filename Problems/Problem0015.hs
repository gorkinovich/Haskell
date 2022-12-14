{--
Copyright (c) 2014 Gorka Suárez García

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
--}

{- ***************************************************************

    Starting in the top left corner of a 2×2 grid, there are 6
    routes (without backtracking) to the bottom right corner.

        1 2 3    1 2 .    1 2 .
        . . 4    . 3 4    . 3 .
        . . 5    . . 5    . 4 5

        1 . .    1 . .    1 . .
        2 3 4    2 3 .    2 . .
        . . 5    . 4 5    3 4 5

    How many routes are there through a 20×20 grid?

*************************************************************** -}
module Problem0015 (main) where

createTable :: (Integral a) => Int -> Int -> a -> [[a]]
createTable r c v = take r (repeat line)
    where line = take c (repeat v)

createSquareTable s v = createTable s s v

getTable :: (Integral a) => Int -> Int -> [[a]] -> a
getTable r c grid = getRow 0 grid
    where getCol _ [] = error "Out of bounds"
          getCol j (l:ls)
              | j == c    = l
              | otherwise = getCol (j + 1) ls
          getRow _ [] = error "Out of bounds"
          getRow i (g:gs)
              | i == r    = getCol 0 g
              | otherwise = getRow (i + 1) gs

setTable :: (Integral a) => Int -> Int -> a -> [[a]] -> [[a]]
setTable r c v grid = setRow 0 grid
    where setCol _ [] = []
          setCol j (l:ls)
              | j == c    = v : ls
              | otherwise = l : setCol (j + 1) ls
          setRow _ [] = []
          setRow i (g:gs)
              | i == r    = (setCol 0 g) : gs
              | otherwise = g : setRow (i + 1) gs

-- ***************************************************************

-- http://en.wikipedia.org/wiki/Pascal's_triangle
pascalFast :: Int -> Int -> Integer
pascalFast m n
    | m < 0 || n < 0 || n > m = error "Out of bounds"
    | n == 0 || n == m = 1
    | otherwise = getTable n m table'
    where table = createSquareTable (m + 1) 1
          coords = [(x, y) | x <- [2..m], y <- [1..x-1]]
          calc t [] = t
          calc t ((x, y):cs) = calc t' cs
              where lv = getTable (y - 1) (x - 1) t
                    rv = getTable y (x - 1) t
                    t' = setTable y x (lv + rv) t
          table' = calc table coords

countPaths :: Int -> Int -> Integer
countPaths r c = pascalFast dist (div dist 2)
    where dist = r + c

main = do putStr "The number of routes in a 20x20 grid are "
          putStrLn $ (show $ countPaths 20 20) ++ "."

-- ***************************************************************

pascal :: Int -> Int -> Integer
pascal m n
    | m < 0 || n < 0 || n > m = error "Out of bounds"
    | n == 0 || n == m = 1
    | otherwise = lv + rv
    where lv = pascal (m - 1) (n - 1)
          rv = pascal (m - 1) n

-- ***************************************************************

pathStep :: (Int, Int) -> (Int, Int) -> Int -> [[Int]] -> [[[Int]]]
pathStep (i, j) (r, c) step grid
    | i == r && j == c = [nextg]
    | i > r || j > c = []
    | otherwise = foldl1 (++) [pathStep (i + a, j + b) (r, c)
                  (step + 1) nextg | (a, b) <- [(0, 1), (1, 0)]]
    where nextg = setTable i j step grid

getPaths :: Int -> Int -> [[[Int]]]
getPaths r c = pathStep orig dest 1 grid
    where grid = createTable r c 0
          orig = (0, 0)
          dest = (r - 1, c - 1)

-- ***************************************************************

pathStepLength :: (Int, Int) -> (Int, Int) -> Integer
pathStepLength (i, j) (r, c)
    | i == r && j == c = 1
    | i > r || j > c = 0
    | otherwise = sum [fpsl x | x <- [(0, 1), (1, 0)]]
    where fpsl (a, b) = pathStepLength (i+a, j+b) (r, c)

getPathsLength :: Int -> Int -> Integer
getPathsLength r c = pathStepLength (0, 0) (r - 1, c - 1)

-- ***************************************************************

showPath [] = putStr "\n"
showPath (p:ps) = do putStrLn $ show p
                     showPath ps

showPaths [] = putStr ""
showPaths (p:ps) = do showPath p
                      showPaths ps