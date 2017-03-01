module Mydate
( nextDate
)where

data Date = Date {year :: Int, month :: Int, day :: Int} deriving (Show)

days = [31,28,31,30,31,30,31,31,30,31,30,31]
leapyear = [31,29,31,30,31,30,31,31,30,31,30,31]
months = ["none", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]



isLeapY :: Int -> Bool
isLeapY y = divisibleBy 400 || (divisibleBy 4 && not (divisibleBy 100))
   where divisibleBy x = mod y x == 0

checkDate :: Date -> Bool
checkDate (Date {year = a, month = b, day = c}) 
          | isLeapY a && b > 0 && b < 13 && c > 0 && c <= (leapyear !! (b-1)) = True
          | (isLeapY a == False) && b > 0 && b < 13 && c > 0 && c <= (days !! (b-1)) = True
          | otherwise = False

nextDate :: Maybe Date -> Maybe Date 
nextDate Nothing = Nothing
nextDate (Just (Date {year = a, month = b, day = c }))
        | (not (checkDate (Date a b c))) = Nothing
        | isLeapY a && c + 1 <= (leapyear !! (b-1)) = Just (Date a b (c+1))
        | (isLeapY a == False) && c + 1 <= (days !! (b-1)) = Just(Date a b (c+1))
        | b + 1 <= 12 = Just (Date a (b+1) 1)
        | otherwise = Just (Date (a+1) 1 1) 

