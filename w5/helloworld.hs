main = do
    putStrLn "give your name"
    name <- getLine
    putStrLn ("your name is " ++ name ++ " which is great!")