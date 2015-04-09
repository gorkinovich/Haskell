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

    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

     Find the sum of all the primes below two million.

*************************************************************** -}
module Problem0010 (main) where

squareRoot :: (Integral a) => a -> a
squareRoot x = truncate $ sqrt $ fromIntegral x

multipleOf :: (Integral a) => a -> a -> Bool
multipleOf a b = (mod a b) == 0

isPrime :: (Integral a) => a -> [a] -> Bool
isPrime n ps = ip n ps
    where upperLimit = squareRoot n + 1
          ip n [] = True
          ip n (x:xs) = if x > upperLimit || n == x then True
                        else if multipleOf n x then False
                        else ip n xs

sumPrimes :: Integer -> Integer
sumPrimes limit = sp limit 2 [] 0
    where sp lmt x ps acc
              | x >= lmt = acc
              | otherwise = sp lmt y (ps ++ [x]) (acc + x)
              where y = head [z | z <- [x + 1..], isPrime z ps]

main = do putStr "The sum of all the primes below two million "
          putStrLn $ "is " ++ show (sumPrimes 2000000) ++ "."