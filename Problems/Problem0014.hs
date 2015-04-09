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

    The following iterative sequence is defined for the set of
    positive integers:

      n -> n/2 (n is even)
      n -> 3n + 1 (n is odd)

    Using the rule above and starting with 13, we generate the
    following sequence:
    
      13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

    It can be seen that this sequence (starting at 13 and
    finishing at 1) contains 10 terms. Although it has not
    been proved yet (Collatz Problem), it is thought that
    all starting numbers finish at 1.

    Which starting number, under one million, produces the
    longest chain?

    NOTE: Once the chain starts the terms are allowed to go
    above one million.

*************************************************************** -}
module Problem0014 (main) where

nextNumber :: (Integral a) => a -> a
nextNumber n = if even n then div n 2 else 3 * n + 1

getSequence :: (Integral a) => a -> [a]
getSequence 1 = [1]
getSequence n = n : getSequence (nextNumber n)

-- ***************************************************************

getSequenceLength :: (Integral a) => a -> Int
getSequenceLength 1 = 1
getSequenceLength n = 1 + getSequenceLength (nextNumber n)

findMLSStep :: (Integral a) => a -> a -> Int -> a -> a
findMLSStep c top len n
    | c >= top  = n
    | otherwise = if clen >= len then
                      findMLSStep (c + 1) top clen c
                  else
                      findMLSStep (c + 1) top len n
    where clen = getSequenceLength c

findMaxLenSeq :: (Integral a) => a -> [a]
findMaxLenSeq top = getSequence n
    where n = findMLSStep 1 top 0 1

main = do putStr "The starting number, under one million,"
          putStr "that produces the longest chain is "
          putStrLn $ (show $ head mls) ++ "."
    where mls = findMaxLenSeq 1000000