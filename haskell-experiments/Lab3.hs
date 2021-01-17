module Lab3 where
import Prelude ( Ord , ( <=) , Eq , (==) , Bool ( True , False ))
import Bool
import Functions
import List


--Questão 01
group :: Eq a => [a] -> [[a]]
group                   =  groupBy (==)

--Questão 2
groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy eq []           =  []
groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                           where (ys,zs) = span (eq x) xs

-- Questão 3

--all ::  (a -> Bool) -> [a] -> Bool
--any :: (a -> Bool) -> [a] -> Bool
-- Questão 4
-- Questão 5
-- Questão 6