module Lab3 where
import Prelude ( Either,Ord , ( <=) , Eq , (==) , (<),(+), (-), Bool ( True , False ), (<=))
import Bool
import Functions
import List
import Maybe

import Lab1

--Questão 01
group :: Eq a => [a] -> [[a]]
group =  groupBy (==)

--Questão 2
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy eq [] =  []
groupBy eq (x:xs)=  (x:ys) : groupBy eq zs where (ys,zs) = span (eq x) xs

-- Questão 3
all :: (a -> Bool) -> [a] -> Bool
all x [] = True
all x (y:ys) = if (x y) then all x ys else False

any :: (a -> Bool) -> [a] -> Bool
any x [] = False
any x (y:ys) = if (x y) then True else any x ys

-- Questão 4
merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
--merge [] [] = []
merge (x:xs) (y:ys) = if (x <= y) then x : merge xs (y:ys) else y : merge (x:xs) ys

-- Questão 5
my_split :: [a] -> ([a], [a])
my_split [] = ([],[])
my_split [a] = ([a],[])
my_split (x:xs:xss) = (x:d,xs:j) where (d,j) = my_split xss

--(x: my_split xss) xs:  
--my_split (x:xs) = x : my_split (drop 1 xs)  

--['1','2',3,4]

-- questão 06
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort xs = if (t<2) then xs else merge m n 
	where 
		t = my_length xs 
		(a,b) = my_split xs 
		m = mergesort a 
		n = mergesort b

-- exercicio 09
my_and :: [Bool] -> Bool
my_and a = foldr (==) True a

--my_or :: [Bool] -> Bool
--my_or a = foldr (==) False a

my_map :: (a -> b) -> [a] -> [b]
my_map f  =  foldr (\x ys -> f x : ys) []

-- exercicio 10
my_iterate :: (a -> a) -> a -> [a]
my_iterate f xs = xs : my_iterate f (f xs)

-- exercicio 11 
uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x,xs)
-- exercicio 12


