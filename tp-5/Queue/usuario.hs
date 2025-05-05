import QueueV1
-- import QueueV2

-- Cuenta la cantidad de elementos de la cola.
lengthQ :: Queue a -> Int
lengthQ q = if isEmptyQ q
             then 0
             else 1 + lengthQ (dequeue q)

-- Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola. Nota: chequear que los elementos queden en el orden correcto.
queueToList :: Queue a -> [a]
queueToList q = if isEmptyQ q
                    then []
                    else firstQ q : queueToList (dequeue q)

-- Inserta todos los elementos de la segunda cola en la primera.
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if isEmptyQ q2
                then q1
                else unionQ (enqueue (firstQ q2) q1) (dequeue q2)