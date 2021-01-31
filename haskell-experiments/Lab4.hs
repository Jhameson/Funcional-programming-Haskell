module Main where
import Prelude (String, Eq((==) ,(/=)), Int, Num((+)), Char, IO, readFile, putStrLn, Show(show), Monad( >>=), pure, undefined)
import Bool
import Functions 
import List
import qualified BSTree as BST

-----------------------------------------------------
data BSTree k v = Empty
                | Branch (k, v) (BSTree k v) (BSTree k v)

empty :: BSTree k v
empty = Empty
------------------------------------------------------

--main :: IO ()
--main = readFile " heyjude . txt " >>= pure . process >>= putStrLn
main :: IO ()
main = readFile " heyjude.txt " >>= putStrLn


-------------------------------------------------------

--Questão 01
my_remove :: Eq a => a -> [a] -> [a]
my_remove x = filter (/=x)

--Questão 02

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
count :: [[Char]] ->BST.BSTree [Char] Int
count xs = foldl f BST.empty xs where 
	f btree plv= if (BST.contains btree plv)
		then BST.update btree plv (+1)                                                                       
		else BST.insert btree (plv,1)
--Questão 05

process :: [Char] -> [Char]
process = makeOutput . countWords . clean where
    clean = undefined
    countWords = undefined
    makeOutput = concat . intercalate " \n" . map f . BST.inOrder
    f ( str , c ) = str ++ ": " ++ show c

countWords :: [String] -> BST.BSTree [String] Int
countWords xs = foldl f BST.empty xs where 
	f btree plv= if (BST.contains btree plv)
		then BST.update btree plv (+1)                                                                       
		else BST.insert btree (plv,1)

clean :: [Char] -> [Char]
clean xs = foldr my_remove xs [',','!','?',')','(']




--Questão 06
--
--
--
--
--
---------------------------------------------------------------------------------------------
--E makeOutput está esperando uma BSTree
-- Lembra que countWords vai receber o conteúdo do arquivo 
--(limpo) e vai só contar as palavras numa BSTree String Int

-- Daí countWords vai quebrar em linhas, quebrar as linhas em palavras, 
--e vai dar concat pra ter a lista de palavras do arquivo

--Depois disso um foldr com algumas operações de BSTree dá conta de 
--construir a BSTree que associa a cada palavra o número de ocorrências dela
----------------------------------------------------------------------------------------------
--
--
--
--
-- exercicio 13
--breadth :: BSTree k v -> [[k,v]]

-- exercicio 14

leaves :: BSTree k v -> Int
leaves  Empty = 0
leaves (Branch a Empty Empty) = 1
leaves (Branch a left  right) = leaves left + leaves right + 0


-- exercicio 15
inOrder :: BSTree k v -> [(k, v)]
inOrder Empty = []
inOrder (Branch n esq dir) = inOrder esq ++ (n : inOrder dir)

preOrder :: BSTree k v -> [(k, v)]
preOrder  Empty = []
preOrder  (Branch n esq dir) = n:preOrder  esq ++ preOrder  dir

postOrder::BSTree k v -> [(k, v)]
postOrder Empty = []
postOrder (Branch n esq dir) = postOrder esq ++ postOrder dir ++ [n]

