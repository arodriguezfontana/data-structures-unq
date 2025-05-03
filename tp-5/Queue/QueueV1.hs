module QueueV1(Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where
-- Una lista donde el elemento que se agrega primero es el primero en salir.

data Queue a = Q [a]
-- Inv. Rep.
-- Sea (Q xs) una cola y xs los elementos de la misma.
-- * En xs los elementos entran por el final de la lista y salen por el inicio.

emptyQ :: Queue a -- O(1)
-- Crea una cola vacía.
emptyQ = Q []

isEmptyQ :: Queue a -> Bool -- O(1)
-- Dada una cola indica si la cola está vacía.
isEmptyQ (Q xs) = null xs

enqueue :: a -> Queue a -> Queue a -- O(n) siendo n la longitud de xs.
-- Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q xs) = Q (xs++[x])

firstQ :: Queue a -> a -- O(1)
-- Dada una cola devuelve el primer elemento de la cola.
firstQ (Q xs) = head xs

dequeue :: Queue a -> Queue a -- O(1)
-- Dada una cola la devuelve sin su primer elemento.
dequeue (Q xs) = Q (tail xs)