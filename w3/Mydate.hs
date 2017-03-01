data Date = Date (Int, Int, Int) deriving (Show)

module Mydate
( checkDate
, nextDate
, prevDate
, distDays
, whichMonth
, giveSundays
, giveDate
)where

days = [31,28,31,30,31,30,31,31,30,31,30,31]
leapyear = [31,29,31,30,31,30,31,31,30,31,30,31]
months = ["none", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]



isLeapY :: Integer -> Bool
isLeapY y = divisibleBy 400 || (divisibleBy 4 && not (divisibleBy 100))
   where divisibleBy x = mod y x == 0

checkDate (a,b,c) 
          | isLeapY a && b > 0 && b < 13 && c > 0 && c <= (leapyear !! (b-1)) = True
          | (isLeapY a == False) && b > 0 && b < 13 && c > 0 && c <= (days !! (b-1)) = True
          | otherwise = False

nextDate (a,b,c)
        | isLeapY a && c + 1 <= (leapyear !! (b-1)) = (a,b,c+1)
        | (isLeapY a == False) && c + 1 <= (days !! (b-1)) = (a,b,c+1)
        | b + 1 <= 12 = (a,b+1,1)
        | otherwise = (a+1,1,1)
        
prevDate (a,b,c)
        | c > 1 = (a,b,c-1)
        | isLeapY a && c - 1 == 0 && b > 1 = (a,b-1, leapyear !! (b-2))
        | (isLeapY a == False) && c - 1 == 0 && b > 1 = (a,b-1, days !! (b-2))
        | otherwise = (a-1,12,31)
        
distDays (a, b, c) (d, e, f)
            | (a, b, c) == (d, e, f) = 0
            | (a, b, c) < (d, e, f) = 1 + distDays (nextDate (a, b, c)) (d, e, f)
            | otherwise = 1 + distDays (prevDate (a, b, c)) (d, e, f)

whichMonth x = if x > 0 && x < 13 then months !! x else "Incorrect month number"

giveSundays (a,b)
   | b == 13 = []
   | a + 7 > (days !! (b-1)) = (a,b):giveSundays(7-((days !! (b-1)) - a),b+1)
   | otherwise = (a,b):giveSundays(a+7,b)
   
giveDate (a,b,c) = (nextDate (a,b,c),prevDate(a,b,c))