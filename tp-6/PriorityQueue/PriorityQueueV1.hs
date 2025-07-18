module PriorityQueueV1(PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ) where

data PriorityQueue a = PQ [a]
-- Inv. Rep.
-- Sea (PQ xs) una cola de prioridad y xs los elementos de la misma.
-- * xs esta ordenada de menor a mayor

emptyPQ :: PriorityQueue a
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ xs) = PQ (insert x xs)

findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PQ xs) = head xs

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ xs) = PQ (tail xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x < y
                    then x : y : ys
                    else y : insert x ys