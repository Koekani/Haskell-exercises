startsOrEnds :: [[Char]] -> Char -> [[Char]]
startsOrEnds words char = [ word | word <- words, (head word == char) || (last word == char)]  