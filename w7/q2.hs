import Control.Concurrent
import Control.Concurrent.Chan
import System.IO

listenC :: Int -> (Chan String) -> IO () 
listenC 0 cha = return ()
listenC k cha = do
   readChan cha >>= print
   listenC (k-1) cha


channel k = do
    ch <- newChan
    forkIO $ do
       writeChan ch "hello world"
       writeChan ch "now i quit"
       writeChan ch "now i really quit"
       m <- newMVar k
       n <- takeMVar m
       listenC n ch

