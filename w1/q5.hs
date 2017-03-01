days = [31,28,31,30,31,30,31,31,30,31,30,31]
giveSundays (a,b)
   | b == 13 = []
   | a + 7 > (days !! (b-1)) = (a,b):giveSundays(7-((days !! (b-1)) - a),b+1)
   | otherwise = (a,b):giveSundays(a+7,b)