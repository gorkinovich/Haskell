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

    By listing the first six prime numbers: 2, 3, 5, 7, 11, and
    13, we can see that the 6th prime is 13.

    What is the 10,001st prime number?

*************************************************************** -}
module Problem0007 (main) where

squareRoot :: (Integral a) => a -> a
squareRoot x = truncate $ sqrt $ fromIntegral x

multipleOf :: (Integral a) => a -> a -> Bool
multipleOf a b = (mod a b) == 0

isPrime :: (Integral a) => a -> Bool
isPrime 2 = True
isPrime n = not $ or [multipleOf n x | x <- 2:[3,5..upperLimit]]
    where upperLimit = squareRoot n + 1

getPrimeNumber :: Int -> Integer
getPrimeNumber index = head $ drop index primesList
    where primesList = [x | x <- [1..], isPrime x]

main = do putStr "The 10,001st prime number is "
          putStrLn $ show (getPrimeNumber 10001) ++ "."