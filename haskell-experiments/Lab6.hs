module Main where

import Prelude (Char, Bool(False, True), (==), Int, pure, (>>=) , IO, readFile, putStrLn, read, show)

import Bool
import Functions
import List 
import Maybe 

import qualified Graph as G

-----------------------------------

lines' :: [Char] -> [[Char]]
lines' = split '\n'


transform :: Maybe Int -> Int
transform a  = case a of 
 Just value -> value                                                                                                                                           
 Nothing -> 0
-----------------------------------

hasCelebrity :: G.Graph -> Maybe Int
hasCelebrity g = if (verify candidate) then candidate else Nothing 
 where
  verify =  maybe False f where
   f _ = and [cond1, cond2] where
   cond1 = G.neighbors g (transform candidate) == [] 
   cond2 =  if or(map (G.hasEdge g (transform candidate)) (G.vertices g)) then False else True
  candidate = removeVertex g (G.vertices g)

{-
4
5 4
2 4
1 4
3 4
-}

--candidate :: G.Graph -> Maybe Int
--candidate g = removeVertex g (G.vertices g)


removeVertex:: G.Graph -> [Int] -> Maybe Int
removeVertex _ [v] = Just v
removeVertex _ [] = Nothing
removeVertex g (x:xs:xss) = if (G.hasEdge g x xs) 
 then  (removeVertex g (xs:xss))
 else  (removeVertex g (x:xss))

------------------------------------

buildGraph :: [[ Char ]] -> G.Graph
buildGraph [] = G.empty
buildGraph (n : es ) = foldr f gn es' where
 es' = map words es
 gn = G.edgeless (read n)
 f e g = G.addEdge g u v where
  (u: v:_) = map read e

process :: [ Char ] -> [ Char ]
process = makeOutput . hasCelebrity . buildGraph . lines' where
 makeOutput = show

-----------------------------------------

main :: IO ()
main = readFile "g1.txt" >>= pure . process >>= putStrLn

------------------------------------------