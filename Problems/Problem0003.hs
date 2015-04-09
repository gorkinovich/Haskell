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

    The prime factors of 13195 are 5, 7, 13 and 29.

    What is the largest prime factor of the number 600851475143?

*************************************************************** -}
module Problem0003 (main) where

squareRoot :: (Integral a) => a -> a
squareRoot x = truncate $ sqrt $ fromIntegral x

multipleOf :: (Integral a) => a -> a -> Bool
multipleOf a b = (mod a b) == 0

isPrime :: (Integral a) => a -> Bool
isPrime 2 = True
isPrime n = not $ or [multipleOf n x | x <- 2:[3,5..upperLimit]]
    where upperLimit = squareRoot n + 1

reduceByFactor :: (Integral a) => a -> a -> a
reduceByFactor n x =
    if multipleOf n x then reduceByFactor (div n x) x
    else n

getFactors :: (Integral a) => a -> [a]
getFactors number = gf number [x | x <- [2..], isPrime x] []
    where gf n (p:ps) rs =
              if p > n then
                  rs
              else if multipleOf n p then
                  gf (reduceByFactor n p) ps (p:rs)
              else
                  gf n ps rs

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = head (getFactors n)

main = do putStr "The largest prime factor of the number "
          putStrLn (show number ++ " = " ++ show result)
    where number = 600851475143
          result = largestPrimeFactor number

-- ***************************************************************

primesList = [x | x <- [1..], isPrime x]

isPrime' :: (Integral a) => a -> Bool
isPrime' n = not $ or [multipleOf n x | x <- [2..(n - 1)]]

isPrime'' :: (Integral a) => a -> Bool
isPrime'' number = ip number 2
    where nextNum 2 = 3
          nextNum n = n + 2
          ip n x = if x >= n then True
                   else if multipleOf n x then False
                   else ip n (nextNum x)

largestPrimeFactor' :: Integer -> Integer
largestPrimeFactor' n = head $ filter selector inverseList
    where inverseList = [n,(n - 1)..1]
          selector x = multipleOf n x && isPrime x

primeFactorOf :: (Integral a) => a -> a -> Bool
primeFactorOf a b = multipleOf a b && isPrime b

largestPrimeFactor'' :: Integer -> Integer
largestPrimeFactor'' n = lpf n
    where lpf x = if primeFactorOf n x then x
                  else lpf (x - 1)

-- ***************************************************************