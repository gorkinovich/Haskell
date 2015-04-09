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

    The sum of the squares of the first ten natural numbers is,
    1^2 + 2^2 + ... + 10^2 = 385

    The square of the sum of the first ten natural numbers is,
    (1 + 2 + ... + 10)^2 = 55^2 = 3025

    Hence the difference between the sum of the squares of the
    first ten natural numbers and the square of the sum is
    3025 - 385 = 2640.

    Find the difference between the sum of the squares of the
    first one hundred natural numbers and the square of the sum.

*************************************************************** -}
module Problem0006 (main) where

findDiffSquares :: (Integral a) => a -> a
findDiffSquares limit = squareOfSum - sumOfSquares
    where sumOfSquares = sum [x ^ 2 | x <- [1..limit]]
          squareOfSum = sum [x | x <- [1..limit]] ^ 2

main = do putStr "The difference between the sum of the squares "
          putStr "of the first one hundred natural numbers and "
          putStrLn $ "the square of the sum is " ++ result ++ "."
    where result = show $ findDiffSquares 100