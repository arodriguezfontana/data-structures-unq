module QueueV3(Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

data Queue a = Q [a] [a]
-- Inv. Rep.
-- Sea (Q fs bs) una cola y fs y bs listas.
-- * Si fs se encuentra vacía, la cola se encuentra vacía.
-- * Los elementos salen por fs y se agregan por bs.

emptyQ :: Queue a
emptyQ = Q [] []

isEmptyQ :: Queue a -> Bool 
isEmptyQ (Q fs bs) = null fs 

enqueue :: a -> Queue a -> Queue
enqueue x (Q fs bs) = if null fs
                        then Q (x:fs) []
                        else Q fs (x:bs)

firstQ :: Queue a -> a
firstQ (Q fs bs) = head fs

dequeue :: Queue a -> Queue a
firstQ (Q fs bs) = if null fs
                    then Q (reverse bs) []
                    else Q (tail fs) bs