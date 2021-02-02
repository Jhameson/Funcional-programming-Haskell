{-
	Todas as funções estão contidas no Lab4, as únicas novas inserções foram
	a função "myCompare", "(!?)" e uma alteração na "process" (a exigida do Lab5)
-}



module Main where

import Prelude (Eq, Bool(True, False), Int, Char, IO, Ord , (==), (/=),  (+),  (-), (>=),
   readFile, putStrLn, 
 show,(>>=), pure,   otherwise)



import Maybe
import Functions
import List ((++), intercalate, concat, foldl, span, dropWhile, filter , sortBy)


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

{-
	#########################################################	
-}
-- LABORATÓRIO 06
 
process' :: [Char] -> [Char]
process' =  makeOutput .  countWords . clean where
 clean = remove' '.' . remove' '!' . remove' '?' . remove' '(' . remove' ')' . remove' ','
 countWords = count' . concat . map words' . lines'
 makeOutput = concat . intercalate "\n" .  map f . sortBy myCompare . BST.inOrder
 f (word, count') = word ++ ": " ++ show  count'


myCompare :: (Ord a, Ord b) => (a,b) -> (a,b) -> Bool
myCompare (_,v1) (_,v2) | v1 >= v2 = True | otherwise = False

{-
	#########################################################	
-}

--------------------------------------
--Exercicio 16
(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x: _) !? 0 = Just x
(_: xs ) !? k = xs !? (k - 1)

-------------------------------------


-- uma_lista = [("dois", 2),("um", 1),("tres", 3)]
--ordena lista = sortBy my_compare lista


------------------------------
--valor (_, n) = n
{-
mis :: Ord a  => [a] -> [a] -> [a]
mis [] y = y
mis x [] = x
mis (x:xs) (y:ys) = if (xs <= ys) 
 then x: mis xs (y:ys) 
 else y: mis (x:xs) ys
-}

main :: IO ()
main = readFile "heyjude.txt" >>=    pure  .   process' >>= putStrLn







