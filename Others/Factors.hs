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
-- Primes example application
-- Make: ghc -main-is Primes Primes.hs
-- ***************************************************************
module Factors (main) where

squareRoot :: (Integral a) => a -> a
squareRoot x = truncate $ sqrt $ fromIntegral x

multipleOf :: (Integral a) => a -> a -> Bool
multipleOf a b = (mod a b) == 0

isPrime :: (Integral a) => a -> Bool
isPrime 2 = True
isPrime n = not $ or [multipleOf n x | x <- 2:[3,5..upperLimit]]
    where upperLimit = squareRoot n + 1

getFactors :: (Integral a) => a -> [a]
getFactors 1 = [1]
getFactors number = reverse temp
    where gf n (p:ps) rs =
              if p > n then
                  rs
              else if multipleOf n p then
                  gf (div n p) (p:ps) (p:rs)
              else
                  gf n ps rs
          temp = gf number [x | x <- [2..], isPrime x] [1]

main = mapM_ putStrLn l2
    where l1 = [(x, getFactors x) | x <- [1..100]]
          l2 = [show x ++ ": " ++ show y | (x, y) <- l1]