import QueueV1
-- import QueueV2

lengthQ :: Queue a -> Int
-- Cuenta la cantidad de elementos de la cola.
lengthQ q = if isEmptyQ q
             then 0
             else 1 + lengthQ (dequeue q)

queueToList :: Queue a -> [a]
-- Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola. Nota: chequear que los elementos queden en el orden correcto.
queueToList q = if isEmptyQ q
                    then []
                    else firstQ q : queueToList (dequeue q)

unionQ :: Queue a -> Queue a -> Queue a
-- Inserta todos los elementos de la segunda cola en la primera.
unionQ q1 q2 = if isEmptyQ q1
                then q2
                else enqueue (firstQ q1) (unionQ (dequeue q2) q2)