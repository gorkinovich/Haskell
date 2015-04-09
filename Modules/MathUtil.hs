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
-- Module: MathUtil
--         This module contains math utility functions.
-- ***************************************************************
module MathUtil where

import qualified Data.List as List

-- ***************************************************************

-- Checks if a conversion from an integral value to
-- double is secure or not.
checkSecureConversion :: (Integral a) => a -> Bool
checkSecureConversion x = truncate y == x
    where y = fromIntegral x :: Double

-- Checks if the integral a is multiple of b.
multipleOf :: (Integral a) => a -> a -> Bool
multipleOf a b = (mod a b) == 0

-- Checks if the integral a is multiple of all the numbers
-- inside the list bs.
multipleOfList :: (Integral a) => a -> [a] -> Bool
multipleOfList a bs = and [multipleOf a x | x <- bs]

-- ***************************************************************

-- Calculates the integer square root in a fast way.
squareRoot :: (Integral a) => a -> a
squareRoot x = truncate $ sqrt $ (fromIntegral x :: Double)

-- Calculates the integer square root in a secure way.
squareRoot' :: (Integral a) => a -> a
squareRoot' v =
    if checkSecureConversion v then squareRoot v
    else secureSquareRoot v

-- Finds a range of values to find the integer square root.
findSqrtRange :: (Integral a) => a -> a -> (a, a)
findSqrtRange v br = fsr 0 1
    where shift 1 = 2
          shift x = x ^ 2
          fsr prev next =
              if (next + br) ^ 2 < v then
                  fsr next (shift next)
              else
                  (br + prev, br + next)

-- Calculates the integer square root in a secure way.
secureSquareRoot :: (Integral a) => a -> a
secureSquareRoot v = if v < 2 then v
                     else step 0
    where step lwr =
              if nlwr ^ 2 == v then nlwr
              else if nupr ^ 2 == v then nupr
              else if nupr - nlwr > 1 then step nlwr
              else nlwr
              where (nlwr, nupr) = findSqrtRange v lwr

-- ***************************************************************

-- Checks if a number is prime or not.
isPrime :: (Integral a) => a -> Bool
isPrime 2 = True
isPrime n = not $ or [multipleOf n x | x <- 2:[3,5..upperLimit]]
    where upperLimit = squareRoot n + 1

-- The list of prime numbers.
primesList = [x | x <- [1..], isPrime x]

-- The list of prime numbers, without number 1.
primesList' = [x | x <- [2..], isPrime x]

-- ***************************************************************

-- Gets the prime factors of a number.
getFactors :: (Integral a) => a -> [a]
getFactors number = reverse $ getFactors' number

-- Gets the prime factors of a number. The result is reversed.
getFactors' :: (Integral a) => a -> [a]
getFactors' 1 = [1]
getFactors' number = gf number [x | x <- [2..], isPrime x] [1]
    where gf n (p:ps) rs =
              if p > n then
                  rs
              else if multipleOf n p then
                  gf (div n p) (p:ps) (p:rs)
              else
                  gf n ps rs

-- Gets the prime factors of a number, without number one.
-- The result is reversed.
getFactors'' :: (Integral a) => a -> [a]
getFactors'' 1 = [1]
getFactors'' number = gf number [x | x <- [2..], isPrime x] []
    where gf n (p:ps) rs =
              if p > n then
                  rs
              else if multipleOf n p then
                  gf (div n p) (p:ps) (p:rs)
              else
                  gf n ps rs

-- Groups a given sorted list of factors.
groupFactors :: (Integral a) => [a] -> [[a]]
groupFactors [] = []
groupFactors lst = gf lst (head lst) [] []
    where gf [] v ac rs = rs ++ [ac]
          gf (x:xs) v ac rs =
              if x == v then
                  gf xs v (x:ac) rs
              else
                  gf xs x [x] (rs ++ [ac])

-- ***************************************************************

-- The multi-node tree data type.
data Tree a = Empty | Node a (TreeList a) deriving Show
type TreeList a = [Tree a]

-- Calculates the number of nodes inside a tree.
treeLength :: Tree a -> Int
treeLength Empty = 0
treeLength t = f t
    where f Empty = 0
          f (Node x []) = 1
          f (Node x ns) = 1 + sum [f n | n <- ns]

-- Makes subgroups from a factors list of the same number.
makeFactorsGroups :: (Integral a) => [a] -> [[a]]
makeFactorsGroups [] = []
makeFactorsGroups [x] = [[x]]
makeFactorsGroups xs = [drop n xs | n <- [0..length xs - 1]]

-- Makes a divisors multi-node tree.
makeDivisorsTree :: (Integral a) => [[a]] -> Tree a
makeDivisorsTree [] = Empty
makeDivisorsTree [x] = Node (product x) []
makeDivisorsTree (x:xs) = Node (product x) (mdst xs [])
    where mdst [] rs = rs
          mdst (y:ys) rs = mdst ys (rs ++ ns)
              where yg = makeFactorsGroups y
                    ns = [makeDivisorsTree (z:ys) | z <- yg]

-- Uses a multi-node tree to obtain the divisors.
getDivisorsFromTree :: (Integral a) => a -> Tree a -> [a]
getDivisorsFromTree _ Empty = []
getDivisorsFromTree v (Node x []) = [div v x]
getDivisorsFromTree v (Node x ys) = r:rs
    where r = div v x
          rs = concat [getDivisorsFromTree r y | y <- ys]

-- ***************************************************************

-- Gets the divisors of a number.
getDivisors :: (Integral a) => a -> [a]
getDivisors n = [x | x <- [1..n], multipleOf n x]

-- Gets the divisors of a number from a factors list.
getDivisorsFromFactors :: (Integral a) => [a] -> [a]
getDivisorsFromFactors [] = []
getDivisorsFromFactors [x] = [x]
getDivisorsFromFactors lst = rs
    where dt = makeDivisorsTree $ groupFactors lst
          rs = getDivisorsFromTree (product lst) dt

-- Gets the divisors of a number from a factors list.
getDivisorsFromFactors' :: (Integral a) => [a] -> [a]
getDivisorsFromFactors' lst = List.nub victims
    where victims = [product x | x <- List.subsequences lst]

-- Gets the divisors of a number from a factors list.
getSortedDivisorsFromFactors :: (Integral a) => [a] -> [a]
getSortedDivisorsFromFactors lst = List.sort victims
    where victims = getDivisorsFromFactors lst

-- Gets the divisors of a number from a factors list.
getSortedDivisorsFromFactors' :: (Integral a) => [a] -> [a]
getSortedDivisorsFromFactors' lst = List.sort victims
    where victims = getDivisorsFromFactors' lst

-- ***************************************************************

-- Calculates a quadratic equation.
quadraticEquation :: (Floating a) => a -> a -> a -> (a, a)
quadraticEquation a b c = (r1, r2)
    where aux = sqrt (b ^ 2 - 4 * a * c)
          r1 = (-b + aux) / (2 * a)
          r2 = (-b - aux) / (2 * a)

-- Calculates an integer quadratic equation.
quadraticEquation' :: (Integral a) => a -> a -> a -> (a, a)
quadraticEquation' a b c = (r1, r2)
    where aux = squareRoot (b ^ 2 - 4 * a * c)
          r1 = div (-b + aux) (2 * a)
          r2 = div (-b - aux) (2 * a)

-- ***************************************************************

-- Gets the triangle number of a position in the sequence.
getTriangleNumber :: Integer -> Integer
getTriangleNumber n = if n < 0 then 0 else div (n * (n + 1)) 2

-- Gets the position in the sequence of a triangle number.
triangleNumberEquation :: Integer -> Integer
triangleNumberEquation n = div (aux - 1) 2
    where aux = squareRoot' (1 + 8 * n)

-- Validates if a number is a triangle number or returns 0.
fromTriangleNumber :: Integer -> Integer
fromTriangleNumber n = if n == getTriangleNumber r then r else 0
    where r = triangleNumberEquation n

-- Checks if a number is a triangle number.
isTriangleNumber :: Integer -> Bool
isTriangleNumber n = n == getTriangleNumber r
    where r = triangleNumberEquation n

-- Gets the nearest triangle numbers of a value.
getTriangleNumbersNear :: Integer -> (Integer, Integer)
getTriangleNumbersNear n = (r, r + 1)
    where r = triangleNumberEquation n

-- ***************************************************************