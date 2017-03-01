import System.Environment
import System.Directory
import System.IO
import System.IO.Error
import Data.List

-- filename = "todo.txt"

main = catch start ioHandler

start = do
    (command:args) <- getArgs
    case lookup command dispatch of
       Just action = action args
       Nothing -> putStr "Invalid arguments"

dispatch :: [(String, [String] -> IO())]
dispatch = [  ("add", add)
            , ("view", view)
            , ("remove", remove)]

readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing


add :: [String] -> IO()
add [fileName, todoItem] = Just (appendFile fileName (todoItem ++ "\n"))

view :: [String] -> IO()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName


ioHandler :: IOError -> IO ()
ioHandler e
    | isDoesNotExistError e = 
        case ioeGetFileName e of Just path -> putStrLn $ "File doesn't exist at: " ++ path
                                 Nothing -> putStrLn $ "File doesn't exist at unknown location."
    | isUserError e = putStrLn $ "Did you forget filename and action?"
    | otherwise = ioError e    