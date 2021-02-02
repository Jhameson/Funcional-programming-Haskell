module Main where

import Prelude (Eq, (==), (/=), Int, (+), (>),
 (<),(<=), Char, IO, readFile, putStrLn, 
 show,(>>=), pure, Ord {-, Ordering(LT, EQ, GT), otherwise-})



import Maybe
import Functions
import List ((++), intercalate, concat, foldl, span, dropWhile, filter, {-length,-} sortBy)


import qualified BSTree as BST
------------------------------

remove' :: Eq a => a -> [a] -> [a]
remove' x = filter (/=x)

split' :: Eq a => a -> [a] -> [[a]] 
split' _ []= []  
split' x xs = ps : split' x qs where
 ys = dropWhile (==x) xs
 (ps,qs) = span (/=x) ys

lines' :: [Char] -> [[Char]]
lines' = split' '\n'

words' :: [Char] -> [[Char]]
words' = split' ' '

count' :: [[Char]] -> BST.BSTree [Char] Int
count' = foldl f BST.empty where
 f bst word = g lkp where
  lkp = BST.lookup bst word
  g Nothing = BST.insert bst (word, 1)
  g (Just _) = BST.update bst word (+1)
------------------------------
 
process' :: [Char] -> [Char]
process' = makeOutput . countWords . clean where
 clean = remove' '.' . remove' '!' . remove' '?' . remove' '(' . remove' ')' . remove' ','
 countWords = count' . concat . map words' . lines'
 makeOutput = concat . intercalate "\n" . map f . BST.inOrder
 f (word, count') = word ++ ": " ++ show count'

--------------------------------------
--------------------------------------
merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) = if (x <= y) then x : merge xs (y:ys) else y : merge (x:xs) ys

my_split :: [a] -> ([a], [a])
my_split [] = ([],[])
my_split [a] = ([a],[])
my_split (x:xs:xss) = (x:d,xs:j) where (d,j) = my_split xss

{-
mergesort :: (Ord a , Ord b) => (a,b) -> (a,b)
mergesort [] = []
mergesort xs = if (t<2) then xs else merge m n 
 where 
  t = length xs 
  (a,b) = my_split xs 
  m = mergesort a 
  n = mergesort b
 -}
--------------------------------------
--------------------------------------
{-
uma_lista = [("dois", 2),("um", 1),("tres", 3)]
-}

{-}
compara :: (Ord a, Ord b) => (a,b) -> (a,b) -> Ordering
compara (a1,b1) (a2,b2) | b1 > b2 =GT | b1 == b2 = EQ  | otherwise = LT
-}

--ordena lista = sortBy compara lista


--------------------------------------
--------------------------------------
main :: IO ()
main = readFile "heyjude.txt" >>=   pure  . sortBy (<). process' >>= putStrLn