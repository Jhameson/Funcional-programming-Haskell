module Main where

import Prelude (Char, Int, Eq((==), (/=)), Num((+)), (++), IO, readFile, putStrLn, Applicative(pure), (>>=), Show(show))

------------------------------
import Maybe
import Functions
import List (filter, span, dropWhile, foldl, intercalate, concat)

------------------------------
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
------------------------------

main :: IO ()
main = readFile "heyjude.txt" >>= pure . process' >>= putStrLn
