takeK :: Int -> String -> [String]
takeK k s
   | length s >= k = take k s : takeK k (tail s)
   | otherwise = []   