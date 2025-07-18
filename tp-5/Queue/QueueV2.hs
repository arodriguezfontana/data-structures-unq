module QueueV2(Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

data Queue a = Q [a]
-- Inv. Rep.
-- Sea (Q xs) una cola y xs los elementos de la misma.
-- * En xs los elementos se agregan por el inicio de la lista y salen por el final.

emptyQ :: Queue a 
emptyQ = Q []

isEmptyQ :: Queue a -> Bool 
isEmptyQ (Q xs) = null xs

enqueue :: a -> Queue a -> Queue a 
enqueue x (Q xs) = Q (x:xs)

firstQ :: Queue a -> a
firstQ (Q xs) = last xs

dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (init xs)