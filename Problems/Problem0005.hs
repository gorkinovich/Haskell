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

    2520 is the smallest number that can be divided by each of
    the numbers from 1 to 10 without any remainder.

    What is the smallest positive number that is evenly
    divisible (divisible with no remainder) by all of the
    numbers from 1 to 20?

*************************************************************** -}
module Problem0005 (main) where

squareRoot :: (Integral a) => a -> a
squareRoot x = truncate $ sqrt $ fromIntegral x

multipleOf :: (Integral a) => a -> a -> Bool
multipleOf a b = (mod a b) == 0

multipleOfList :: (Integral a) => a -> [a] -> Bool
multipleOfList a bs = and [multipleOf a x | x <- bs]

isPrime :: (Integral a) => a -> Bool
isPrime 2 = True
isPrime n = not $ or [multipleOf n x | x <- 2:[3,5..upperLimit]]
    where upperLimit = squareRoot n + 1

getFactors :: (Integral a) => a -> [a]
getFactors 1 = [1]
getFactors number = gf number [x | x <- [2..], isPrime x] []
    where gf n (p:ps) rs =
              if p > n then
                  rs
              else if multipleOf n p then
                  gf (div n p) (p:ps) (p:rs)
              else
                  gf n ps rs

makeCountList :: (Eq a) => [a] -> [(a, Int)]
makeCountList [] = []
makeCountList (x:xs) = makeLst (x, 1) xs
    where makeLst (a, b) [] = [(a, b)]
          makeLst (a, b) (y:ys) =
              if a == y then makeLst (a, b + 1) ys
              else (a, b):(makeLst (y, 1) ys)

getFromCountList :: (Eq a) => a -> [(a, Int)] -> Maybe (a, Int)
getFromCountList n [] = Nothing
getFromCountList n ((a, b):xs) =
    if a == n then Just (a, b)
    else getFromCountList n xs

remFromCountList :: (Eq a) => a -> [(a, Int)] -> [(a, Int)]
remFromCountList n [] = []
remFromCountList n ((a, b):xs) =
    if a == n then remFromCountList n xs
    else (a, b):(remFromCountList n xs)

joinCountList :: (Eq a) => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
joinCountList [] [] = []
joinCountList [] ys = ys
joinCountList xs [] = xs
joinCountList ((a, b):xs) ys = nxtStp $ getFromCountList a ys
    where nxtStp Nothing = (a, b):(joinCountList xs ys)
          nxtStp (Just (c, d)) = ab':(joinCountList xs ys')
              where ab' = (a, max b d)
                    ys' = remFromCountList c ys

makeJoinedCountList :: (Eq a) => [[a]] -> [(a, Int)]
makeJoinedCountList [] = []
makeJoinedCountList lst = foldl accFnc [] fctLst
    where accFnc acc x = joinCountList acc x
          fctLst = [makeCountList x | x <- lst]

multIntCountList :: (Integral a) => [(a, Int)] -> a
multIntCountList [] = 0
multIntCountList lst = micl lst
    where micl [] = 1
          micl ((a, b):xs) = (a ^ b) * micl xs

findSmallestNumber :: Integer -> Integer
findSmallestNumber limit = multIntCountList fctLst
    where factors = [getFactors x | x <- [1..limit]]
          fctLst = makeJoinedCountList factors

main = do putStr "The smallest positive number that is evenly "
          putStr "divisible (with no remainder) by all of the "
          putStrLn $ "numbers from 1 to 20 = " ++ show result
    where result = findSmallestNumber 20

-- ***************************************************************

primesList = [x | x <- [1..], isPrime x]

findSmallestNumber' :: Integer -> Integer
findSmallestNumber' limit = head candidates
    where range = [1..limit]
          candidates = [x | x <- [1..], multipleOfList x range]

-- ***************************************************************