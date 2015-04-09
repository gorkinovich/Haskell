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

    If we list all the natural numbers below 10 that are
    multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of
    these multiples is 23.

    Find the sum of all the multiples of 3 or 5 below 1000.

*************************************************************** -}
module Problem0001 (main) where

multipleOf :: (Integral a) => a -> a -> Bool
multipleOf a b = (mod a b) == 0

multipleOf3Or5 :: (Integral a) => a -> Bool
multipleOf3Or5 x = (x `multipleOf` 3) || (x `multipleOf` 5)

multiplesOf3And5Under :: (Integral a) => a -> [a]
multiplesOf3And5Under n = takeWhile (< n) multiples
    where multiples = [x | x <- [1,2..], multipleOf3Or5 x]

main = do putStr "The sum of all the multiples of 3 or 5 "
          putStrLn ("below 1000 is " ++ show result ++ ".")
    where result = sum $ multiplesOf3And5Under 1000