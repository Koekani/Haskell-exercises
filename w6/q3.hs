import Control.Monad.Writer  

type Birds = Int  
type Pole = (Birds,Birds)

landLeft :: Int -> Pole -> Writer [String] Pole
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = writer ((left + n, right), ["Number of birds that landed left: " ++ show n])
    | otherwise                    = writer ((left, right),  ["Too many birds landed left, falling"])  

landRight :: Int -> Pole -> Writer [String] Pole
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = writer ((left, right + n), ["Number of birds that landed right: " ++ show n])
    | otherwise                    = writer ((left, right), ["Too many birds landed right, falling"])  