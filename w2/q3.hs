
mergeLists [] _ = []
mergeLists _ [] = []
mergeLists (x:xs) (y:ys) = x:y:mergeLists xs ys
  