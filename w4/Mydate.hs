module Mydate
( checkDate
, nextDate
, prevDate
, distDays
, giveSurroundingDates
)where

data Date = Date {year :: Int, month :: Int, day :: Int} deriving (Show)

days = [31,28,31,30,31,30,31,31,30,31,30,31]
leapyear = [31,29,31,30,31,30,31,31,30,31,30,31]
months = ["none", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]



isLeapY :: Int -> Bool
isLeapY y = divisibleBy 400 || (divisibleBy 4 && not (divisibleBy 100))
   where divisibleBy x = mod y x == 0

checkDate (Date {year = a, month = b, day = c}) 
          | isLeapY a && b > 0 && b < 13 && c > 0 && c <= (leapyear !! (b-1)) = True
          | (isLeapY a == False) && b > 0 && b < 13 && c > 0 && c <= (days !! (b-1)) = True
          | otherwise = False

nextDate (Date {year = a, month = b, day = c })
        | isLeapY a && c + 1 <= (leapyear !! (b-1)) = Date a b (c+1)
        | (isLeapY a == False) && c + 1 <= (days !! (b-1)) = Date a b (c+1)
        | b + 1 <= 12 = Date a (b+1) 1
        | otherwise = Date (a+1) 1 1 

prevDate (Date {year = a, month = b, day = c})
        | c > 1 = Date a b (c-1)
        | isLeapY a && c - 1 == 0 && b > 1 = Date a (b-1) (leapyear !! (b-2))
        | (isLeapY a == False) && c - 1 == 0 && b > 1 = Date a (b-1) (days !! (b-2))
        | otherwise = Date (a-1) 12 31

distDays (Date a b c) (Date d e f)
            | (a, b, c) == (d, e, f) = 0
            | (a, b, c) < (d, e, f) = 1 + distDays (nextDate (Date a b c)) (Date d e f)
            | otherwise = 1 + distDays (prevDate (Date a b c)) (Date d e f)

giveSurroundingDates (Date a b c) = (nextDate (Date a b c), prevDate(Date a b c))
