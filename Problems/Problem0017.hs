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

    If the numbers 1 to 5 are written out in words: one, two,
    three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19
    letters used in total.

    If all the numbers from 1 to 1000 (one thousand) inclusive
    were written out in words, how many letters would be used?

    NOTE: Do not count spaces or hyphens. For example, 342 (three
    hundred and forty-two) contains 23 letters and 115 (one
    hundred and fifteen) contains 20 letters. The use of "and"
    when writing out numbers is in compliance with British usage.

*************************************************************** -}
module Problem0017 (main) where

import Data.Char

-- ***************************************************************

words0to19 = ["zero", "one", "two", "three", "four",
              "five", "six", "seven", "eight", "nine",
              "ten", "eleven", "twelve", "thirteen",
              "fourteen", "fifteen", "sixteen",
              "seventeen", "eighteen", "nineteen"]

words20to90 = ["", "", "twenty", "thirty", "forty", "fifty",
               "sixty", "seventy", "eighty", "ninety"]

word1H = " hundred "
word1T = " thousand "
word1M = " million "
word1B = " billion "

word1Ha = word1H ++ "and "

num1H = 100
num1T = 10^3
num1M = 10^6
num1B = 10^9
limit = 10^12

get :: (Integral a) => [b] -> a -> b
get [] _ = error "get: Out of bounds"
get (x:xs) 0 = x
get (x:xs) i = if i > 0 then get xs (i - 1)
               else error "get: Out of bounds"

-- ***************************************************************

toLetters2D :: Integer -> String
toLetters2D n
    |  0 <= n && n <  20 = get words0to19 n
    | 20 <= n && n < 100 = if n0 == 0 then r1 else r2
    | otherwise = error "toLetters2D: Out of bounds"
        where (n1, n0) = divMod n 10
              r1 = get words20to90 n1
              r2 = r1 ++ "-" ++ (get words0to19 n0)

toLettersEx :: Integer -> Bool -> String
toLettersEx n andFlag
    | n >= limit = error "toLettersEx: Out of bounds"
    | n >= num1B = f0 bn1 bn0 word1B
    | n >= num1M = f0 mn1 mn0 word1M
    | n >= num1T = f0 tn1 tn0 word1T
    | n >= num1H = if andFlag then v0 else v1
    | otherwise  = toLetters2D n
    where (bn1, bn0) = divMod n num1B
          (mn1, mn0) = divMod n num1M
          (tn1, tn0) = divMod n num1T
          (hn1, hn0) = divMod n num1H
          f0 n1 n0 w = toLettersEx n1 False ++ w ++
                       (if n0 == 0 then "" else toLettersEx n0 andFlag)
          v0 = (get words0to19 hn1) ++ 
               (if hn0 == 0 then word1H else word1Ha ++toLetters2D hn0)
          v1 = (get words0to19 hn1) ++ word1H ++ toLetters2D hn0

toLetters :: Integer -> String
toLetters n = toLettersEx n True

-- ***************************************************************

numberLen :: String -> Integer
numberLen s = sum [1 | c <- s, isAlpha c]

solve :: Integer -> Integer
solve top = sum [numberLen (toLetters x) | x <- [1..top]]

-- ***************************************************************

main = do putStr "Number of letters of the written numbers "
          putStrLn $ "from 1 to 1000: " ++ (show $ solve 1000)