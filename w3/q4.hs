takeK :: Int -> String -> [String]
takeK k s
   | length s >= k = take k s : takeK k (tail s)
   | otherwise = []   

gap (x:xs) (y:ys) wrong maxwrong
   | y == [] && x != []  = false
   | x == [] && y == []  = true
   |
   
isSubString n x y 
   | x elem (takeK (length x) y)
   |   