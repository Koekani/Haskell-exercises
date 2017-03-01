triangles :: [(Int, Int, Int)]
triangles = do
   (a,b,c) <- [ (x,y,z) | z <- [1..10], y <- [1..z], x <- [1..y] ]
   guard ((a^2 + b^2 == c^2) && (a + b + c == 24))
   return (a,b,c)