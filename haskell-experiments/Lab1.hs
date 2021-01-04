module Lab1 where

import Prelude (Eq ,(+) , (*) , (.), Int, (==), Bool(True, False), Num, flip)
import List ( foldl , foldr , filter)


import Bool (cond)


-------------------------------------------------------------------------
-- ##### QUESTÃO 01 #####
-- função length
length :: [a] -> Int
length = foldl f 0 where f acc _ = acc +1

-- função (++)
(++) :: [a]->[a]->[a]
[] ++ ys = ys
xs ++ [] = xs
(x:xs) ++ ys = foldr (:) ys xs



-- função concat 
concat :: [a]->[a]->[a]
concat [] a = a
concat b [] = b
concat xs ys = foldr (:)  ys xs

-- função reverse
reverse :: [a] -> [a]
reverse  = foldl (flip (:)) []


-- ##### QUESTÃO 02 #####
squares :: Num a => [a] -> [a]
squares [] = []
squares (x:xs) = x*x : squares xs

-- ##### QUESTÃO 03 #####
count :: Eq a => a -> [a] -> Int
count x [ ] = 0
count x ys = length ( filter ( x == ) ys )



-------------------------------------------------------------------------
--EXERCICIO 01
-- IMPLEMENTAR AND E OR
and :: Bool -> Bool -> Bool
and False _ = False
and _ False = False
and True True = True

or :: Bool -> Bool -> Bool
or True _ = True
or _ True = True
or False False = False

-- IMPLEMENTAR isInfixOf
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf (x : xs ) (y: ys ) = cond (x == y) ( isInfixOf xs ys ) False