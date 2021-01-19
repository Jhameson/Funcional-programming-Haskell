module Lab3 where
import Prelude ( Ord , ( <=) , Eq , (==) , Bool ( True , False ), (<=))
import Bool
import Functions
import List
import Lab2



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

--any :: (a -> Bool) -> [a] -> Bool
--any y (x:xs) = 

-- Questão 4
merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
--merge [] [] = []
merge (x:xs) (y:ys) = if (x <= y) then x : merge xs (y:ys) else y : merge (x:xs) ys

-- Questão 5
my_split :: [a] -> ([a], [a])
my_split (x:xs) = x : my_split (drop 1 xs)


--all ::  (a -> Bool) -> [a] -> Bool
--any :: (a -> Bool) -> [a] -> Bool
-- Questão 4
-- Questão 5
-- Questão 6

-- exercicio 09
--my_and :: Bool -> Bool -> Bool
--my_and a b = foldr and a b

--m_map :: (a -> b) -> [a] -> [b]
--m_map _ [] = []
--m_map f xs =  foldr f xs

