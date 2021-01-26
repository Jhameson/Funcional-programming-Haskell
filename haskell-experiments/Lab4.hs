module Main where
import Prelude (String, Eq, (==), (/=), Int, (+), Char, IO, readFile, putStrLn, show,( >>=), pure, undefined)
import Bool
import Functions 
import List
import qualified BSTree as BST

data BSTree k v = Empty
                | Branch (k, v) (BSTree k v) (BSTree k v)


main :: IO ()
main = readFile " heyjude.txt " >>= putStrLn

empty :: BSTree k v
empty = Empty

--main :: IO ()
--main = readFile " heyjude . txt " >>= pure . process >>= putStrLn

--Questão 01
my_remove :: Eq a => a -> [a] -> [a]
my_remove x = filter (/=x)

--Questão 02
--split' :: Eq a => a -> [a] -> [[a]]
--split' a [] = []
--split' a xs = x : split' a (drop 1 y) where (x,y) = span (/= a) xs

split' :: Eq a => a -> [a] -> [[a]]
split' _ [] = []
split' x [y] = cond (x == y) [] [[y]]
split' x ys = ps : split' x qs where
    (ps, qs) = f ys
    f = span (/=x) . dropWhile (==x)

--Questão 03
lines' :: [Char] -> [[Char]]
lines' = split '\n'

words' :: [Char] -> [[Char]]
words' = split ' '
--Questão 04
count :: [[Char]] -> BST.BSTree [Char] Int
count xs = foldr BSTree. 

--Questão 05

clean :: [Char] -> [Char]
clean xs = foldr my_remove xs [',','!','?',')','(']

--countWords :: String -> BST.BSTree

--Questão 06
--E makeOutput está esperando uma BSTree
-- Lembra que countWords vai receber o conteúdo do arquivo 
--(limpo) e vai só contar as palavras numa BSTree String Int

-- Daí countWords vai quebrar em linhas, quebrar as linhas em palavras, 
--e vai dar concat pra ter a lista de palavras do arquivo

--Depois disso um foldr com algumas operações de BSTree dá conta de 
--construir a BSTree que associa a cada palavra o número de ocorrências dela

-- exercicio
inOrder :: BSTree k v -> [(k, v)]
inOrder Empty = []
inOrder (Branch keyval lt rt) = inOrder lt ++ (keyval : inOrder rt)

