module Main where

import Prelude (Show, show, Eq,(==),Bool(False, True), 
	Ord, Char, undefined,IO, (<), (<=),print, putStrLn, 
	readFile, writeFile, getContents,(>>=))

import System.Environment(getArgs)
import System.IO
import Maybe 
import Functions 
import List (intercalate, concat, lines, head, length, concat)


---------------------------------------------------------
-- EXERCÍCIO 19
instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  Nothing == _ = False
  Just _ == Nothing = False
  Just x == Just y = x == y
-- EXERCÍCIO 21

instance Ord a => Ord (Maybe a) where
    Nothing <= _ = True
    Just _ <= Nothing = False
    Just x <= Just y = x <= y

-- EXERCÍCIO 22
instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just x) = "Just " ++ show x
---------------------------------------------------------
my_split :: [a] -> ([a], [a])
my_split [] = ([],[])
my_split [a] = ([a],[])
my_split (x:xs:xss) = (x:d,xs:j) where (d,j) = my_split xss

merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) = if (x <= y) then x : merge xs (y:ys) else y : merge (x:xs) ys

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort xs = if (t<2) then xs else merge m n 
 where 
  t = length xs 
  (a,b) = my_split xs 
  m = mergesort a 
  n = mergesort b
---------------------------------------------------------"\n"
program1 :: IO ()
program1 = getArgs >>= print

program2 :: IO ()
program2 = getArgs >>= readFile . head  >>=  putStrLn

program3 :: IO ()
program3 = getArgs >>= readFile . head >>=  putStrLn . concat . intercalate "\n" . mergesort . lines



-- A MODIFICAÇÃO ESTÁ NO ARQUIVO Lab4.hs (última linha)
program4 :: IO ()
program4 = getArgs >>= treatArgs where
treatArgs [ from , to ] = readFile from >>= writeFile to
treatArgs _ = putStrLn "You must pass exactly two arguments."

program5 :: IO ()
program5 = getArgs >>= treatArgs where
  treatArgs [first] = readFile first >>= putStrLn
  treatArgs _ = getContents >>= putStrLn

program6 :: IO()
program6 = getArgs >>= readFile . head  >>=  putStrLn

------------------------------------------------------------

main = program5