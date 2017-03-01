days = [31,28,31,30,31,30,31,31,30,31,30,31]
leapyear = [31,29,31,30,31,30,31,31,30,31,30,31]
isLeapY :: Integer -> Bool
isLeapY y = divisibleBy 400 || (divisibleBy 4 && not (divisibleBy 100))
   where divisibleBy x = mod y x == 0
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
