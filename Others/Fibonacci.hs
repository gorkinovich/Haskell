{--
Copyright (c) 2012 Gorka Suárez García

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

-- ***************************************************************
-- Fibonacci example application
-- Make: ghc -main-is Fibonacci Fibonacci.hs
-- ***************************************************************
module Fibonacci (main) where

heavyFibonacci :: Integer -> Integer
heavyFibonacci 0 = 0
heavyFibonacci 1 = 1
heavyFibonacci n = heavyFibonacci(n - 1) + heavyFibonacci(n - 2)

fibonacci :: Integer -> Integer
fibonacci x = let fib 0 (v0, _) = v0
                  fib i (v0, v1) = fib (i - 1) (v1, v0 + v1)
              in fib x (0, 1)

separation m n = replicate (m - (length $ show n)) ' '

fibNumToStr i n maxw1 maxw2 = hd ++ tl
    where hd = "Fib(" ++ show i ++ ")" ++ (separation maxw1 i)
          tl = " = " ++ (separation maxw2 n) ++ show n

main = do putStrLn "Fibonacci test:"
          putStrLn (foldl1 (\a b -> a ++ "\n" ++ b) buffer)
    where numbers = [fibonacci x | x <- [1..100]]
          maxw = length $ show $ maximum numbers
          numToStr i n = fibNumToStr i n 3 maxw
          bufferData = zip [1..100] numbers
          buffer = [numToStr i n | (i, n) <- bufferData]