module Queue (Queue,empty,enqueue,dequeue) where

import Prelude ()
import Maybe
import List (reverse)

data Queue a = Q [a] [a]

empty :: Queue a
empty = Q [] []

enqueue :: Queue a -> a -> Queue a
enqueue  (Q front back) x= Q front (x:back)


dequeue :: Queue a -> Maybe (a , Queue a)
dequeue queue = case queue of
 Q [] [] -> Nothing
 Q (x:f) b -> Just (x, Q f b)
 otherwise -> dequeue (rotate queue)
 where rotate (Q [] back) = Q (reverse back) []
 