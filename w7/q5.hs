import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)  

type Birds = Int  
type Pole = (Birds,Birds)

landLeft :: Int -> Pole -> Writer [([String], Pole)] IO
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = writer ((left + n, right), ["Number of birds that landed left: " ++ show n])
    | otherwise                    = writer ((left, right),  ["Too many birds landed left, falling"])  

landRight :: Int -> Pole -> Writer Writer [([String], Pole)] IO
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = writer ((left, right + n), ["Number of birds that landed right: " ++ show n])
    | otherwise                    = writer ((left, right), ["Too many birds landed right, falling"])  