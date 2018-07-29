module MinSetPath where

minsetpath :: [(Int,Int)] -> Int
minsetpath list  = sumdiffs (minpath [] list)

sumdiffs :: [Int] -> Int
sumdiffs path = sum (zipWith (\a b -> abs (a - b)) (init path) (tail path))

minpath :: [Int] -> [(Int,Int)] -> [Int]
minpath p []  = p
minpath [] [x,y] | maxx == miny  = [maxx]
                 | minx == maxy  = [minx]
                 | maxx < miny   = [maxx,miny]
                 | minx > maxy   = [minx,maxy]
                 | minx == maxx  = [minx]
                 | miny == maxy  = [miny]
                 | minx >= miny  = [minx]
                 | otherwise     = [miny]          -- arbitrary
                 where minx = min (fst x) (snd x)
                       maxx = max (fst x) (snd x)
                       miny = min (fst y) (snd y)
                       maxy = max (fst y) (snd y)

minpath [] (x:y:xs) | minx <= miny && maxx >= miny && maxx <= maxy  = minpath [] ((miny,maxx):xs)  -- first two ranges overlap, collapse them into one
                    | miny <= minx && maxy >= minx && maxy <= maxx  = minpath [] ((minx,maxy):xs)
                    | minx <= miny && maxx >= maxy  = minpath [] (y:xs)  -- one of the first two ranges is a superset of the other, ignore it
                    | miny <= minx && maxy >= maxx  = minpath [] (x:xs)
                    | otherwise  = minpath (minpath [] [x,y]) xs
                    where minx = min (fst x) (snd x)
                          maxx = max (fst x) (snd x)
                          miny = min (fst y) (snd y)
                          maxy = max (fst y) (snd y)

minpath p (x:xs) | p' < minx  = minpath (p ++ [minx]) xs
                 | p' > maxx  = minpath (p ++ [maxx]) xs
                 | minx <= p' && p' <= maxx  = minpath p xs
                 where minx = min (fst x) (snd x)
                       maxx = max (fst x) (snd x)
                       p'   = last p

