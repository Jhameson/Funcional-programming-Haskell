module Main where

import Prelude (Ord, (<=), (>=), (>) ,(-), Bool(True, False), IO, putStrLn, (<$>), 
 pure, (<*>), undefined, max, foldr, foldl,otherwise,Int)

import Bool
import Functions
import List (head, tail,length, drop, filter, (++))
import Maybe


maximum :: Ord a => [a] -> Maybe a
maximum [] = Nothing
maximum xs = Just (foldr max (head xs) (tail xs))

--maxBy:: (a -> a -> Bool) -> a -> a -> a


maximalBy :: (a -> a -> Bool) -> [a] ->  Maybe a
maximalBy _ [] = Nothing
maximalBy _ [x] = Just x
maximalBy eq (x:xs) =  Just (foldl (maxby eq) x xs) where
 maxby eq a b = if eq a b then a else b
 

lis :: Ord a => [a] -> [a]
lis [] = []
lis [x] = [x]
lis xs = fromMaybe [] (maximalBy cmp (s <$> [0.. n - 1] <*> pure xs )) where
 n = length xs
 cmp ps qs = length ps >= length qs
 s 0 [] = []
 s 0 [ y] = [ y]
 s 0 ( y: ys ) = y : ms where
  candidates = filter ((y <=) . head ) $ s <$> [0.. k - 1] <*> pure ys
  k = length ys
  ms = fromMaybe [] $ maximalBy cmp candidates
 s i ys = s 0 $ drop i ys



-- exercicio 32
repeat' :: a -> [a]                                                                                                                                                
repeat' x = fix (x:)

cycle' :: [a] -> [a]                                                                                                      
cycle' = \xs -> fix (xs ++)                                                                                                                                        



 --exercicio 33 
iterate' :: (a -> a) -> a -> [a]
iterate' f x = fix $ (x:) . map(f)


-- exercicio 35
rangeF :: Int -> [Int]
rangeF 0 = [0]
range x = fix(-1)(x:)


main = putStrLn "Lab10"