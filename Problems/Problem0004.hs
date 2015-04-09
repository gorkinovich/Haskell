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

    A palindromic number reads the same both ways. The largest
    palindrome made from the product of two 2-digit numbers is
    9009 = 91 × 99.

    Find the largest palindrome made from the product of two
    3-digit numbers.

*************************************************************** -}
module Problem0004 (main) where

isPalindrome :: (Integral a) => a -> Bool
isPalindrome x = (take half y) == (take half $ reverse y)
    where y = show x
          half = div (length y) 2

makePlndLst :: Integer -> [(Integer, Integer, Integer)]
makePlndLst lmt =
    let fstPlnd x 0 = (x, 0, 0)
        fstPlnd x y = if isPalindrome z then (x, y, z)
                      else fstPlnd x (y - 1)
            where z = x * y
    in [fstPlnd x x | x <- [lmt, lmt - 1..1]]

findMaxPlnd :: [(Integer, Integer, Integer)] -> Integer
findMaxPlnd [] = 0
findMaxPlnd (x:xs) = fmp xs x
    where fmp [] (_, _, mz) = mz
          fmp ((x, y, z):xs) (mx, my, mz) =
              if my > x then mz
              else if mz < z then fmp xs (x, y, z)
              else fmp xs (mx, my, mz)

findPalindrome :: Int -> Integer
findPalindrome digits =
    if digits < 1 then 0
    else let lmt = read (replicate digits '9') :: Integer
         in findMaxPlnd $ makePlndLst lmt

main = do putStr "The largest palindrome made from the product "
          putStrLn ("of two 3-digit numbers = " ++ show result)
    where result = findPalindrome 3