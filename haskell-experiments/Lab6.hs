module Main where

import Prelude (Char, Bool(False), pure, (>>=) , IO, readFile, putStrLn, read, show, undefined)

import Bool
import Functions
import List 
import Maybe

import qualified Graph as G

-----------------------------------

lines' :: [Char] -> [[Char]]
lines' = split '\n'

buildGraph :: [[ Char ]] -> G.Graph
buildGraph [] = G.empty
buildGraph (n : es ) = foldr f gn es' where
 es' = map words es
 gn = G.edgeless (read n)
 f e g = G.addEdge g u v where
  (u: v:_) = map read e


hasCelebrity :: G.Graph -> Bool
hasCelebrity g = verify candidate where
 verify =  maybe False f where
  f c = and [ cond1 , not cond2 ] where
  cond1 = undefined
  cond2 = undefined
 candidate = undefined


process :: [ Char ] -> [ Char ]
process = makeOutput . hasCelebrity . buildGraph . lines where
 makeOutput = show

-----------------------------------------

main :: IO ()
main = readFile " g1 . txt " >>= pure . process >>= putStrLn

------------------------------------------