import System.Environment
import System.Directory
import System.IO
import Data.List
import System.Random

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("remove", remove)
           , ("view", view)
           , ("randomLines", randomLines)
           ]
 
randomLines :: [String] -> IO ()
randomLines [k] = do
   contents <- readFile "todo.txt" ReadMode
   let todoTasks = lines contents
       number = read k
   if k >= length todoTasks
       view
   else
      let randomL = pick k (shuffle todoTasks)
          numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] randomL
      putStr $ unlines numberedTasks  

pick :: Int -> [a] -> [a]
pick 0 _ = []
pick n (x:xs) = x : pick (k-1) (shuffle xs)
    
   

add :: [String] -> IO ()
add [args] = appendFile "todo.txt" (args ++ "\n" )

remove :: [String] -> IO ()  
remove [numberString] = do  
    handle <- openFile "todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt"

view :: [String] -> IO ()
view _ = do
    contents <- readFile "todo.txt"  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks

main = do
   (command:args) <- getArgs
   let (Just action) = lookup command dispatch
   action args
