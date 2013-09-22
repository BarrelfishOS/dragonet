module Util.Misc (
    splitBy, minusL,
    fst3, snd3, thd3,
) where


-- Split list by delimiter into multiple lists
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs
          f _ _ = undefined

-- Setminus on unordered lists (inefficient)
minusL :: (Eq a) => [a] -> [a] -> [a]
minusL a b = filter (not . (flip elem b)) a

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,a,_) = a

thd3 :: (a,b,c) -> c
thd3 (_,_,a) = a

