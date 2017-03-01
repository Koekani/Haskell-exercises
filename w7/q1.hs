import Test.QuickCheck

takeK :: Int -> String -> [String]
takeK k s
   | length s >= k = take k s : takeK k (tail s)
   | otherwise = []   

--test to see that right amount of substrings are created
prop_amount k s = (length s) >= k && k > 0 ==> length (takeK k s) == (length s) + 1 - k

prob_amount = quickCheck (prop_amount :: Int -> [Char] -> Property)

--test to check that subsstrings heads + last substrings tail should create original string
prop_heads k s = (length s) >= k && k > 0 ==> 
   [head a | a <- (takeK k s)] ++ tail (last (takeK k s)) == s

prob_heads = quickCheck (prop_heads :: Int -> [Char] -> Property)