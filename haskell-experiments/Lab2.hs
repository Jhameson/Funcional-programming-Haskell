module Lab2 where
import Prelude ( Eq , Fractional , (/), Ord, (<=))
import Bool
import Functions
import List
import Maybe


--Questão 01
my_repeat :: a -> [a]
my_repeat a = a : my_repeat a
----------------------------------------------------
--Questão 02
my_cycle :: [a] -> [a]
my_cycle []  = []
my_cycle xs = xs ++ my_cycle xs
----------------------------------------------------
--Questão 03
my_intercalate :: a -> [a] -> [a]
my_intercalate _ [] = []
my_intercalate _ [y] = [y] 
my_intercalate x (y:ys) = y : x : my_intercalate x ys
--intercalate x y = (x:y)  : intercalate x y
----------------------------------------------------
--Questão 04
--safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
----------------------------------------------------
--Questão 05
----------------------------------------------------


--Exercícios
--insertion
insertion :: Ord a => a -> [a]->[a]
insertion y [] = [y]
insertion y (x:xs) = if y<=x then y:x:xs else x:insertion y xs
--insertionSort
insertionSort :: Ord a=> [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insertion x (insertionSort xs) 
