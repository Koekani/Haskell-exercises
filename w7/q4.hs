import Control.Concurrent 

mostDist::[String] -> [String] -> IO()
mostDist s1s s2s = do
  m <- newEmptyMVar
  n <- newEmptyMVar
  forkIO $ do
    putMVar m (frequencies2 (generateCandidates [] s1s) s1s)
  forkIO $ do
    putMVar n (frequencies2 (generateCandidates [] s1s) s2s)
  resultS1 <- takeMVar m
  resultS2 <- takeMVar n
  putStrLn $ show (zipWith (\(x,sx) (y,sy) -> (x-y, sx)) resultS1 resultS2)

distScore c s1s s2s = 
   ((s1count-s2count), c)
   where (s1count,c1) = countFreqs2 c s1s 0
         (s2count,c2) = countFreqs2 c s2s 0

frequencies2 [] _ = []
frequencies2 (x:xs) ys =
  (countFreqs2 x ys 0):(frequencies2 xs ys)

countFreqs2 x [] n = (n,x)
countFreqs2 x (y:ys) n = countFreqs2 x ys (count2 x y n)

count2 _ [] k = k
count2 _ [y] k = k
count2 (x1:[x2]) (y1:y2:ys) k 
 | x1 == y1 && x2 == y2 = k+1
 | otherwise = count2 (x1:[x2]) (y2:ys) k

generateCandidates cs [] = cs
generateCandidates cs (s:ss) =
  (generateCandidates (get2strings cs s) ss)

get2strings cs [] = cs
get2strings cs [x] = cs
get2strings cs (x1:x2:xs) 
 | (x1:[x2]) `elem` cs = get2strings cs (x2:xs)
 | otherwise = get2strings ((x1:[x2]):cs) (x2:xs)