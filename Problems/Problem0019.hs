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

    You are given the following information, but you may prefer
    to do some research for yourself.

        + 1 Jan 1900 was a Monday.
        + Thirty days has September,
          April, June and November.
          All the rest have thirty-one,
          Saving February alone,
          Which has twenty-eight, rain or shine.
          And on leap years, twenty-nine.
        + A leap year occurs on any year evenly divisible by 4,
          but not on a century unless it is divisible by 400.

    How many Sundays fell on the first of the month during the
    twentieth century (1 Jan 1901 to 31 Dec 2000)?

*************************************************************** -}
module Problem0019 (main) where

isDiv :: (Integral a) => a -> a -> Bool
isDiv a b = (0 == mod a b)

inRange :: (Integral a) => a -> a -> a -> Bool
inRange x min max = min <= x && x <= max

-- ***************************************************************

type Year = Integer
type Month = Int
type Day  = Int
type WeekDay = Int
type Date = (Year, Month, Day)

-- ***************************************************************

daysByYear  = 365
daysByYear' = 366

daysByMonth  = [31,28,31,30,31,30,31,31,30,31,30,31]
daysByMonth' = [31,29,31,30,31,30,31,31,30,31,30,31]

months = ["January","February","March","April",
          "May","June","July","August",
          "September","October","November","December"]

sundayId    = 1
mondayId    = 2
tuesdayId   = 3
wednesdayId = 4
thursdayId  = 5
fridayId    = 6
saturdayId  = 7

days = ["Sunday","Monday","Tuesday","Wednesday",
        "Thursday","Friday","Saturday"]

monthToString :: Int -> String
monthToString m = if isValid then value else ""
    where isValid = inRange m 1 12
          value = months !! (m - 1)

dayToString :: Int -> String
dayToString d = if isValid then value else ""
    where isValid = inRange d 1 7
          value = days !! (d - 1)

-- ***************************************************************

isLeapYear :: (Integral a) => a -> Bool
isLeapYear y = (fid 4) && ((fid 400) || not (fid 100))
    where fid b = isDiv y b

getDaysByYear :: (Integral a) => a -> Int
getDaysByYear y = if isLeapYear y then daysByYear'
                  else daysByYear

getDaysByYear' :: (Integral a) => a -> Integer
getDaysByYear' y = toInteger $ getDaysByYear y

getDaysByMonth :: (Integral a) => a -> [Int]
getDaysByMonth y = if isLeapYear y then daysByMonth'
                   else daysByMonth

-- ***************************************************************

dateToInt :: Date -> Integer
dateToInt (y, m, d)
    | y < 0     = -((toInteger d) + dm + (yti' y))
    | otherwise = (toInteger d) + dm + (yti y)
    where dm = toInteger $ sum $ take (m - 1) (getDaysByMonth y)
          yti x = if x <= 0 then 0 else dby + (yti (x - 1))
              where dby = getDaysByYear' (x - 1)
          yti' x = if x >= -1 then 0 else dby + (yti' (x + 1))
              where dby = getDaysByYear' (x + 1)

getYearFromInt :: Integer -> Integer
getYearFromInt n
    | n < 0     = fndy' n (-1) 0
    | otherwise = fndy n 0 0
    where fndy m y c = if v < m then fndy m (y+1) v else y
              where v = c + getDaysByYear' y
          fndy' m y c = if v > m then fndy' m (y-1) v else y
              where v = c - getDaysByYear' y

remYearFromInt :: Integer -> Integer
remYearFromInt n = abs(n - m)
    where y = getYearFromInt n
          m = dateToInt (y, 0, 0)

remYearFromInt' :: Integer -> Integer -> Integer
remYearFromInt' n y = abs(n - m)
    where m = dateToInt (y, 0, 0)

intToDate :: Integer -> Date
intToDate n = (y, m, d)
    where y = getYearFromInt n
          md = fromIntegral $ remYearFromInt' n y
          dbm = getDaysByMonth y
          gdm md' m'
              | md' > d'  = gdm (md' - d') (m' + 1)
              | otherwise = (m', md')
              where d' = dbm !! (m' - 1)
          (m, d) = gdm md 1

-- ***************************************************************

-- (1900, 1, 1) -> Monday
firstDay1900 :: Date
firstDay1900 = (1900, 1, 1)

firstDay1900' = dateToInt firstDay1900

getWeekDay :: Date -> WeekDay
getWeekDay d = fromIntegral ((mod delta 7) + 1)
    where delta = (dateToInt d) - (firstDay1900' - 1)

getWeekDay' :: Integer -> WeekDay
getWeekDay' n = fromIntegral ((mod delta 7) + 1)
    where delta = n - (firstDay1900' - 1)

-- ***************************************************************

findNextDay :: WeekDay -> Integer -> Integer
findNextDay wd n
    | wd == v   = n
    | otherwise = findNextDay wd (n + 1)
    where v = getWeekDay' n

-- findSundaysOnFirst 1901 2000 = 171
findSundaysOnFirst :: Year -> Year -> Integer
findSundaysOnFirst y1 y2 = toInteger $ length lst
    where d1 = dateToInt (y1, 1, 1)
          d2 = dateToInt (y2, 12, 31)
          d1a = findNextDay sundayId d1
          d1b = d1a + 7
          lst = [(y,m,d) | (y,m,d) <- [intToDate x | x <-
                 [d1a,d1b..d2]], d == 1]

-- findSundaysOn1st 1901 2000 = 171
findSundaysOn1st :: Year -> Year -> Integer
findSundaysOn1st y1 y2 = toInteger $ length lst
    where d1 = (y1, 1, 1)
          d2 = (y2, 12, 31)
          getLst (ya,ma,da) (yb,mb,db)
              | ya <= yb = (ya,ma,da) : next
              | otherwise  = []
              where aux = ma + 1
                    ma' = if aux > 12 then 1 else aux
                    ya' = if aux > 12 then ya + 1 else ya
                    next = getLst (ya',ma',da) (yb,mb,db)
          lst = [x | x <- getLst d1 d2, sundayId == getWeekDay x]

main = do putStr "The number of Sundays that fell on the first "
          putStr "of the month during the 20th century is "
          putStrLn (show result ++ ".")
    where result = findSundaysOn1st 1901 2000