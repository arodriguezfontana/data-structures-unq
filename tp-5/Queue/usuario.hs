import QueueV1

lengthQ :: Queue a -> Int
lengthQ q = if isEmptyQ q
             then 0
             else 1 + lengthQ (dequeue q)

queueToList :: Queue a -> [a]
queueToList q = if isEmptyQ q
                    then []
                    else firstQ q : queueToList (dequeue q)

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if isEmptyQ q2
                then q1
                else unionQ (enqueue (firstQ q2) q1) (dequeue q2)