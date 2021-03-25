module Lab10 where

import Prelude (Ord, (<=), (>=), (>) ,(-), Bool, IO, putStrLn, (<$>), 
 pure, (<*>), undefined, max, foldr)
import Bool
import Functions
import List (head, tail)
import Maybe


maximum :: Ord a => [a] -> Maybe a
maximum [] = Nothing
maximum xs = Just (foldr max (head xs) (tail xs))

maximalBy :: (a -> a -> Bool) -> [a] -> a
maximalBy f xs = foldr f xs
