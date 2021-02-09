
module Main where
import Prelude (Char,Bool(False),pure,(>>=),IO,readFile,putStrLn,read,show,undefined,Int,error,(==),Bool(True,False))
import Bool
import Functions
import List
import Maybe

import qualified Graph as G



buildGraph :: [[ Char ]] -> G.Graph
buildGraph [] = G.empty
buildGraph ( n : es ) = foldr f gn es' where
         es' = map words es
         gn = G.edgeless(read n)
         f e g = G.addEdge g u v where
          (u:v:_) = map read e



--function elimine-vertices

delVertices:: G.Graph -> [Int] -> Maybe Int
delVertices g [v] = Just v
delVertices g [] = Nothing
delVertices g (x:xs:xss) = if(G.hasEdge g x xs)
                             then (delVertices g (xs:xss))
                           else (delVertices g (x:xss))

 
hasCelebrity :: G.Graph -> Maybe Int
hasCelebrity g = if(verify candidate) then (candidate) else (Nothing)
 where
  verify =  maybe False f where
   f c = and [cond1 , not cond2] where
   cond1 =  G.neighbors g (extract candidate) == []
   cond2=if or(map(G.hasEdge g(extract candidate))(G.vertices g))==True then True else False
  candidate = delVertices g (G.vertices g) 

process :: [Char] -> [Char]
process = makeOutput .hasCelebrity. buildGraph . lines where makeOutput = show

extract :: Maybe Int -> Int
extract a  = case a of
     Just value -> value
     Nothing -> 0

main :: IO ()
main = readFile "g1.txt" >>= pure . process >>= putStrLn

