module Util.Misc (
    splitBy
) where

-- Split list by delimiter into multiple lists
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs
          f _ _ = undefined

