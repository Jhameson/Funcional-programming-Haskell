module Main where

import Prelude (Functor,Bool(True,False),IO, putStrLn, getLine, (<$>), (<*>), pure,
 (*>), (>>=), (&&),Char, (==),(>=), (>>), undefined, return,fmap, Applicative)

import Bool 
import Functions 
import List 
import SeqTree
import Data.Char
import Data.Either



isStrong :: [Char] -> Bool
isStrong xs = if and[a,b,c,d] then True else False where
 a = or(map isLower xs)
 b = or(map isUpper xs)
 c = or(map isDigit xs)
 d = length xs >=8
------------------------------
ask :: [Char] -> IO [Char]
ask _ = putStrLn  "Type a password: " *> getLine

------------------------------
getPasswd :: IO ([Char], [Char])
getPasswd = do
 p1 <- ask ""
 p2 <- ask ""
 return(p1,p2)

------------------------------
validPasswd :: IO [Char]
validPasswd = getPasswd >>= f where
 f(p1,p2) = cond(p1==p2)
  (pure p1)
  (incorrect *> validPasswd)
 incorrect = putStrLn "<<<<<<<<< They don’t match! Try again. >>>>>>>>>>"

------------------------------

strongPasswd :: IO [Char]
strongPasswd = validPasswd >>= f where
 f (xs) = cond(isStrong xs) (pure xs) (incorrect *> strongPasswd)
 incorrect = putStrLn "<<<<<<<<<  Weak password! Try again. >>>>>>>>>>"


-- ################################################ --

--EXERCICIO 23                                                                                                                                                      
--instance Functor SeqTree where
-- fmap f (Leaf a xs) = Leaf (f a) (fmap (fmap f) xs)  

--EXERCICIO 24                                                                                                                                                      
--instance Functor (Either a) where
--	fmap _ (Left x) = Left x
--	fmap f (Right y) = Right (f y) 

--EXERCICIO 25
sequences :: [a]->[[a]]
sequences (x:xs) = (map (x:) (sequences xs)) ++ (sequences xs)

--EXERCICIO 26
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

--EXERCICIO 27
--sequenceA :: Aplicative f => [f a] -> f [a]
--sequenceA [] = pure []
--sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

--EXERCICIO 30
--instance Applicative (Either y) where                                                                                                                            
-- pure h = Right h                                                                                                                                             
-- Right g <*> Right h = Right (g h)    

-- ################################################ --

main = putStrLn "<<<< Create a password >>>>" >> strongPasswd
