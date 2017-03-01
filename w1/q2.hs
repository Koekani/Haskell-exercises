days = [31,28,31,30,31,30,31,31,30,31,30,31]
leapyear = [31,29,31,30,31,30,31,31,30,31,30,31]
isLeapY :: Integer -> Bool
isLeapY y = divisibleBy 400 || (divisibleBy 4 && not (divisibleBy 100))
   where divisibleBy x = mod y x == 0
checkDate (a,b,c) 
          | isLeapY a && b > 0 && b < 13 && c > 0 && c <= (leapyear !! (b-1)) = True
          | (isLeapY a == False) && b > 0 && b < 13 && c > 0 && c <= (days !! (b-1)) = True
          | otherwise = False