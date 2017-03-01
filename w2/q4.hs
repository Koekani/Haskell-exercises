quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 


sortStrings :: (Ord a) => [[a]] -> [[a]]
sortStrings [] = []
sortStrings (x:xs) =
    let smallerSorted = sortStrings [ a | a <- xs, length a < length x || (length a == length x && a <= x)]
        biggerSorted = sortStrings [ a | a <- xs, length a > length x || (length a == length x && a > x)]
    in smallerSorted ++ [x] ++ biggerSorted