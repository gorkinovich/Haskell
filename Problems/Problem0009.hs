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

    A Pythagorean triplet is a set of three natural numbers,
    a < b < c, for which: a^2 + b^2 = c^2

    For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

    There exists exactly one Pythagorean triplet for which
    a + b + c = 1000.

    Find the product abc.

*************************************************************** -}
module Problem0009 (main) where

type Triangle = (Integer, Integer, Integer)
type Triangles = [Triangle]

isTriplet :: (Integral a) => a -> a -> a -> Bool
isTriplet a b c = a < b && b < c && ((a ^ 2 + b ^ 2) == (c ^ 2))

isTriplet' :: (Integral a) => (a, a, a) -> Bool
isTriplet' (a, b, c) = isTriplet a b c

candidates1k :: Triangles
candidates1k = [(a, b, c) | c <- [335..998], b <- [1..c-1],
                            a <- [1..b-1], a + b + c == 1000]

findMagicTriplet :: Triangles -> (Triangle -> Bool) -> Triangle
findMagicTriplet ts tf = fmt [x | x <- ts, tf x]
    where fmt [] = (0, 0, 0)
          fmt xs = head xs

main = do putStr "The Pythagorean triplet for which a + b + c "
          putStrLn $ "= 1000 is " ++ show (a, b, c) ++ "."
          putStrLn $ "The product is: " ++ show (a * b * c)
    where (a, b, c) = findMagicTriplet candidates1k isTriplet'