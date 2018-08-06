module MinSetPath where

minsetpath :: [(Int,Int)] -> Int
minsetpath list  = minpath 0 list

maxofmins :: (Int,Int) -> (Int,Int) -> Int
maxofmins x y  = max (fst x) (fst y)

minofmaxs :: (Int,Int) -> (Int,Int) -> Int
minofmaxs x y  = min (snd x) (snd y)

overlaps :: (Int,Int) -> (Int,Int) -> Bool
x `overlaps` y  = maxofmins x y <= minofmaxs x y

intersection :: (Int,Int) -> (Int,Int) -> (Int,Int)
intersection x y  = ((maxofmins x y), (minofmaxs x y))

dist :: (Int,Int) -> (Int,Int) -> Int
dist x y  = (maxofmins x y) - (minofmaxs x y)

minpath :: Int -> [(Int,Int)] -> Int
minpath t []  = t
minpath t [x] = t
minpath t (x:y:xs) | x `overlaps` y  = minpath t (z:xs)     where z = intersection x y
minpath t (x:y:xs) | snd x < fst y   = minpath (t+d) (z:xs) where d = dist x y; z = (fst y, fst y)
minpath t (x:y:xs) | otherwise       = minpath (t+d) (z:xs) where d = dist x y; z = (snd y, snd y)

