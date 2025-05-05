module PriorityQueueV1(PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ) where

data PriorityQueue a = PQ [a]
-- Inv. Rep.
-- Sea (PQ xs) una cola de prioridad y xs los elementos de la misma.
-- * xs esta ordenada de menor a mayor

emptyPQ :: PriorityQueue a
-- Propósito: devuelve una priority queue vacía.
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool
-- Propósito: indica si la priority queue está vacía.
isEmptyPQ (PQ xs) = null xs

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
-- Propósito: inserta un elemento en la priority queue.
insertPQ x (PQ xs) = PQ (insert x xs)

findMinPQ :: Ord a => PriorityQueue a -> a
-- Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue. Precondición: parcial en caso de priority queue vacía.
findMinPQ (PQ xs) = head xs

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
-- Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo). Precondición: parcial en caso de priority queue vacía.
deleteMinPQ (PQ xs) = PQ (tail xs)

-- SUBTAREA

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x < y
                    then x : y : ys
                    else y : insert x ys