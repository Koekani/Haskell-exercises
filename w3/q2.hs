import Data.Char

encode :: Int -> Char -> Char
encode shift = chr . (+ shift) . ord

ceasar :: [Int] -> String -> String
ceasar shifts = zipWith encode $ cycle shifts