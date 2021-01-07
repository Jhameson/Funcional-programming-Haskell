module RevLab1 where

import Prelude (Int, (+), (*), Eq((==)))
import List (foldl, foldr)

m_length :: [a] -> Int
m_length = foldl f 0 where f acc x = acc + 1

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr f ys xs where x acc = x : acc

m_concat :: [[a]] -> [a]
m_concat = foldr f [] where f xs acc = xs ++ acc

reverse :: [a] -> [a]
reverse = foldl f [] where f acc x = x : acc

m_squares :: [Int] -> [Int]
--m_squares = map f where f x = x * x
m_squares a = zipWith (*) [a] [a]

count :: Eq a => a-> [a] -> Int
count x = foldl f 0 where f acc y =  acc + if x == y then 1  else acc

isInfixOf :: Eq a => [a] -> [a] ->Bool
isInfixOf [] [] = True
isInfixOf _ [] = False
isInfixOf xs ys = p ││ q where
	p = isPrefixOf xs ys
	q = isInfixOf xs (tail ys)