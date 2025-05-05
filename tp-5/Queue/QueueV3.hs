module QueueV3(Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

data Queue a = Q [a] [a]
-- Inv. Rep.
-- Sea (Q fs bs) una cola y fs y bs listas.
-- * Si fs se encuentra vacía, la cola se encuentra vacía.
-- * Los elementos salen por fs y se entran por bs.

emptyQ :: Queue a -- O(1)
-- Crea una cola vacía.
emptyQ = Q [] []

isEmptyQ :: Queue a -> Bool -- O(1) 
-- Dada una cola indica si la cola está vacía.
isEmptyQ (Q fs bs) = null fs 

enqueue :: a -> Queue a -> Queue -- O(1)
-- Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q fs bs) = if null fs
                        then 
                        else

firstQ :: Queue a -> a -- O(1)
-- Dada una cola devuelve el primer elemento de la cola.
firstQ (Q fs bs) = head fs

dequeue :: Queue a -> Queue a -- O(1)
-- Dada una cola la devuelve sin su primer elemento.
firstQ (Q fs bs) = 