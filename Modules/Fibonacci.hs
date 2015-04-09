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
-- Module: Fibonacci
--         This module is used to calculate fibonacci numbers.
-- ***************************************************************
module Fibonacci (
    heavyFibonacci,
    fibonacci,
    fibonacciList,
    fibonacciList2,
    fibonacciList3
) where

heavyFibonacci :: Integer -> Integer
heavyFibonacci 0 = 0
heavyFibonacci 1 = 1
heavyFibonacci n = heavyFibonacci (n - 1) + heavyFibonacci(n - 2)

fibonacci :: Integer -> Integer
fibonacci x =
    let fib 0 (v0, _) = v0
        fib i (v0, v1) = fib (i - 1) (v1, v0 + v1)
    in fib x (0, 1)

fibonacci' :: Integer -> Integer
fibonacci' x =
    let fib i n (v0, v1) = if i == n then v0
                           else fib (i + 1) n (v1, v0 + v1)
    in fib 0 x (0, 1)

fibonacciList :: Integer -> Integer -> [Integer]
fibonacciList a b =
    let fibl i n rs =
            if i == n then reverse (v0:vs)
            else fibl (i + 1) n ((v0 + v1):rs)
            where v1:v0:vs = rs
    in fibl a b [fibonacci (a + 1), fibonacci a]

fibonacciList2 :: [Integer] -> [Integer]
fibonacciList2 [] = []
fibonacciList2 (x:xs) =
    let fibl (y:[]) (v:vs) = reverse vs
        fibl (y:ys) rs = fibl ys ((v0 + v1):rs)
            where v1:v0:vs = rs
    in fibl (x:xs) [fibonacci (x + 1), fibonacci x]

fibonacciList3 :: Integer -> (Integer -> Bool) -> [Integer]
fibonacciList3 x pred =
    let fibl i p rs =
            if not (p v1) then reverse (v0:vs)
            else fibl (i + 1) p ((v0 + v1):rs)
            where v1:v0:vs = rs
    in fibl x pred [fibonacci (x + 1), fibonacci x]