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

    By starting at the top of the triangle below and moving to
    adjacent numbers on the row below, the maximum total from top
    to bottom is 23.

           3              3
          7 4            7 .
         2 4 6          . 4 .
        8 5 9 3        . . 9 .

    That is, 3 + 7 + 4 + 9 = 23.

    Find the maximum total from top to bottom of the triangle
    below:

                      75
                     95 64
                    17 47 82
                   18 35 87 10
                  20 04 82 47 65
                 19 01 23 75 03 34
                88 02 77 73 07 63 67
               99 65 04 28 06 16 70 92
              41 41 26 56 83 40 80 70 33
             41 48 72 33 47 32 37 16 94 29
            53 71 44 65 25 43 91 52 97 51 14
           70 11 33 28 77 73 17 78 39 68 17 57
          91 71 52 38 17 14 91 43 58 50 27 29 48
         63 66 04 68 89 53 67 30 73 16 69 87 40 31
        04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

    NOTE: As there are only 16384 routes, it is possible to solve
    this problem by trying every route. However, Problem 67, is
    the same challenge with a triangle containing one-hundred
    rows; it cannot be solved by brute force, and requires a
    clever method! ;o)

*************************************************************** -}
module Problem0018 (main) where

movl :: Int
movl = 0

movr :: Int
movr = 1

test0 :: [[Int]]
test0 = [[3],
         [7,4],
         [2,4,6],
         [8,5,9,3]]

test1 :: [[Int]]
test1 = [[75],
         [95,64],
         [17,47,82],
         [18,35,87,10],
         [20,04,82,47,65],
         [19,01,23,75,03,34],
         [88,02,77,73,07,63,67],
         [99,65,04,28,06,16,70,92],
         [41,41,26,56,83,40,80,70,33],
         [41,48,72,33,47,32,37,16,94,29],
         [53,71,44,65,25,43,91,52,97,51,14],
         [70,11,33,28,77,73,17,78,39,68,17,57],
         [91,71,52,38,17,14,91,43,58,50,27,29,48],
         [63,66,04,68,89,53,67,30,73,16,69,87,40,31],
         [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]

type Node = (Int, [Int])
type Table = [[Node]]

-- ***************************************************************

createTable :: [[Int]] -> Table
createTable [] = []
createTable (x:xs) = (f x) : createTable xs
    where f [] = []
          f (y:ys) = (y, []) : f ys

solveRow :: [Node] -> [Node] -> [Node]
solveRow [] _ = []
solveRow x [] = x
solveRow x y = current : solveRow xs ((rv,rl):ys)
    where ((xv,_):xs) = x
          ((lv,ll):(rv,rl):ys) = y
          current = if rv < lv then (xv + lv, movl : ll)
                    else (xv + rv, movr : rl)

solvePath :: Table -> [Node]
solvePath [] = []
solvePath (x:xs) = solveRow x nextLine
    where nextLine = solvePath xs

getPath :: [[Int]] -> Node
getPath nums = head $ solvePath $ createTable nums

getPathValues :: [[Int]] -> [Int] -> [Int]
getPathValues nums path = f 0 nums path
    where f c (l:ls) [] = [l !! c]
          f c (l:ls) (d:ds) = (l !! c) : f (c + d) ls ds

-- ***************************************************************

solve nums = (rsum, rmvs, getPathValues nums rmvs)
    where (rsum, rmvs) = getPath nums

main = do putStr "The maximum total from top to bottom of "
          putStrLn $ "the triangle is: " ++ show rsum
          putStrLn $ "Final path: " ++ show rlst
    where (rsum, rmvs, rlst) = solve test1