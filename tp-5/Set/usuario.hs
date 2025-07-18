import SetV1

data Tree a = NodeT a (Tree a) (Tree a) | EmptyT deriving Show

losQuePertenecen :: Eq a => [a] -> Set a -> [a] 
losQuePertenecen [] _ = []
losQuePertenecen (x:xs) s = singularSi x (belongs x s) ++ losQuePertenecen xs s

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (listToSet xs)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT s si sd) = unionS s (unionS (unirTodos si) (unirTodos sd))

listToSet :: Eq a => [a] -> Set a
listToSet [] = emptyS
listToSet (x:xs) = addS x (listToSet xs)

singularSi :: a -> Bool -> [a] -- O(1)
singularSi x True = [x]
singularSi _ _ = []