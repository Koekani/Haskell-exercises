import Test.QuickCheck

takeK :: Int -> String -> [String]
takeK k s
   | length s >= k = take k s : takeK k (tail s)
   | otherwise = []   

--test to see that right amount of substrings is created
prop_amount k s = (length s) >= k && k > 0 ==> length (takeK k s) == (length s) + 1 - k

prob_amount = quickCheck (prop_amount :: Int -> [Char] -> Property)