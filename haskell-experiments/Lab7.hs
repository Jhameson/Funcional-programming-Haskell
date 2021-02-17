{-
 Exercício 17
 Em comparação com a definição das notas de aula, usar foldl na função mergeALL é desvantajoso.
 Porque cria uma estrutura em árvore, e a estrutura tende para a esquerda.
 Portanto, ele é desequilibrado e as oportunidades de usá-lo são cada vez menores.
-}


module Lab7 where

import Prelude (Ord, (<=),(>), Show, Int, 
 error,(-), div, (!!), snd, otherwise, 
 (+), (<), (>=), (++), (.), (/), ($))


import List (length, drop, head, filter, sortBy, foldr)
import Bool
import Maybe

import qualified Heap as MnH
import qualified MaxHeap as MxH


data MedianHeap k v = MedianHeap Int Int (MxH.Heap k v) (MnH.Heap k v)


filterLE :: Ord k => k -> [(k,v)]->[(k,v)]
filterLE k = filter (\(k',_) -> k' <= k)

filterGT :: Ord k => k -> [(k,v)]->[(k,v)]
filterGT k = filter (\(k',_) -> k' < k)

------------------------------------------
median :: Ord k => [(k, v)] -> Maybe (k,v)
median [] = Nothing
median xs = Just (k,v) where
 (k,v) = xs !? div (length xs) 2


(!?) :: [a] -> Int ->  a
[] !? _ = error "error"
(x: _) !? 0 = x
(_: xs ) !? k = xs !? (k - 1)
------------------------------------------
{-
xs = [(1,2),(3,4),(5,6),(7,8),(9,10)]
-}

fromList :: Ord k => [(k,v)]-> MedianHeap k v
fromList xs = MedianHeap le gtl mxh mnh where
 Just (k,_) = median xs
 ls = filterLE k xs
 gt = filterGT k xs
 le = length ls
 gtl = length gt
 mxh = foldr (\kv acc -> MxH.insert acc kv) MxH.empty ls
 mnh = foldr (\kv acc -> MnH.insert acc kv) MnH.empty gt

------------------------------------------

lookup :: Ord k => MedianHeap k v -> Maybe (k,v)
lookup (MedianHeap 0 0 _ _) = Nothing
lookup (MedianHeap lel gtl mxh mnh) = cond (lel >= gtl) l r where
 l = MxH.lookup mxh
 r = MnH.lookup mnh


insert :: Ord k => MedianHeap k v -> (k,v) -> MedianHeap k v
insert (MedianHeap 0 0 mxh mnh) kv = MedianHeap 1 0 (MxH.insert mxh kv) mnh
insert h@(MedianHeap lel gtl mxh mnh) kv@(k,_) = rebalance $ nh where
 nh = cond (k<=k') l r
 l = MedianHeap (lel r 1) gtl (MxH.insert mxh kv) mnh
 r = MedianHeap lel (gtl + 1) mxh (MnH.insert mnh kv)
 Just (k',_) = lookup h



rebalance :: Ord k => MedianHeap k v -> MedianHeap k v
rebalance h@(MedianHeap lel gtl mxh mnh)
 | (lel < gtl) = MedianHeap (lel + 1) (gtl - 1) (MxH.insert mxh mnV) (MnH.pop mnh)
 | (lel > (gtl +1)) = MedianHeap (lel - 1) (gtl +1) (MxH.pop mxh) (MnH.insert mnh mxV)
 | otherwise = h 
 where
 	Just mxV = MxH.lookup mxh
 	Just mnV = MnH.lookup mnh