isLeapY :: Integer -> Bool
isLeapY y = divisibleBy 400 || (divisibleBy 4 && not (divisibleBy 100))
   where divisibleBy x = mod y x == 0
giveLeapYears n y
   | n == 0 = []
   | isLeapY y = y:giveLeapYears (n-1) (y+1)
   | otherwise = giveLeapYears n (y+1)