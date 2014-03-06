module Util.Misc (
    splitBy, joinBy, minusL, partListBy,
    fst3, snd3, thd3,
) where

import qualified Data.List as L
import Data.Function
import qualified Data.Map as M

-- Split list by delimiter into multiple lists
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs
          f _ _ = undefined

-- Concatenate lists with delimiter
joinBy :: [[a]] -> [a] -> [a]
joinBy [] _ = []
joinBy l sep = L.foldl1 (\a b -> a ++ sep ++ b) l


-- Setminus on unordered lists (inefficient)
minusL :: (Eq a) => [a] -> [a] -> [a]
minusL a b = filter (not . (flip elem b)) a

-- Partition a list into multiple labeled sublists according to function
partListBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
partListBy f xs = M.toList $ foldl
    (\m x ->
        let k = f x
            l = if M.member k m then m M.! k else []
        in
            M.insert k (x:l) m) M.empty xs


fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,a,_) = a

thd3 :: (a,b,c) -> c
thd3 (_,_,a) = a

