module QueueV1(Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

data Queue a = Q [a]
-- Inv. Rep.
-- Sea (Q xs) una cola y xs los elementos de la misma.
-- * En xs los elementos se agregan por el final de la lista y salen por el inicio.

emptyQ :: Queue a
emptyQ = Q []

isEmptyQ :: Queue a -> Bool 
isEmptyQ (Q xs) = null xs

enqueue :: a -> Queue a -> Queue a 
enqueue x (Q xs) = Q (xs++[x])

firstQ :: Queue a -> a 
firstQ (Q xs) = head xs

dequeue :: Queue a -> Queue a 
dequeue (Q xs) = Q (tail xs)