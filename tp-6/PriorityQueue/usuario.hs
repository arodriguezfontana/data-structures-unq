import PriorityQueueV1

heapSort :: Ord a => [a] -> [a]
heapSort xs = pqToList (listToPQ xs)

pqToList :: Ord a => PriorityQueue a -> [a]
pqToList pq = if isEmptyPQ pq
                then []
                else findMinPQ pq : pqToList (deleteMinPQ pq)

listToPQ :: Ord a => [a] -> PriorityQueue a
listToPQ [] = emptyPQ
listToPQ (x:xs) = insertPQ x (listToPQ xs)