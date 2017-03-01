import Data.List
import Control.Parallel.Strategies

genkstrings :: Int -> [String] -> [String]
genkstrings k [] = []
genkstrings 1 (s:ss) = [ [c] | c <- s ] ++ genkstrings 1 ss
genkstrings k (s:ss)
  | length (s:ss) < k = []
  | otherwise = concat [kStartWith k c ss | c <- s ]
                ++
                genkstrings k ss

kStartWith k c ss =
 map (c :) $ genkstringsNogap (k-1) ss

genkstringsNogap 0 _ = []
genkstringsNogap 1 (s:ss) = [ [c] | c <- s ]
genkstringsNogap k (s:ss) = concat $ [kStartWithNoGap k c ss | c <- s ]

let kStartWithNoGap k c ss = map (c:) (genkstringsNogap (k-1) ss) as
    cs = kStartWithNoGap `using` parList rdeepseq