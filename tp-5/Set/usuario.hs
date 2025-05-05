import SetV1
-- import SetV2

data Tree a = NodeT a (Tree a) (Tree a) | EmptyT deriving Show

-- V1/V2 O(n*m) siendo n la longitud de xs y m la cantidad de elementos del set. Por cada elemento de xs hago singularSi O(1) + belongs O(n).
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a] 
losQuePertenecen [] _ = []
losQuePertenecen (x:xs) s = singularSi x (belongs x s) ++ losQuePertenecen xs s

-- V1 O(n^2). Hago setToList O(1) + listToSet O(n^2).
-- V1 O(n^2). Hago setToList O(n^2) + listToSet O(n). El costo más grande substrae a el otro.
-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (listToSet xs)

-- V1 
-- V2 
-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT s si sd) = unionS s (unionS (unirTodos si) (unirTodos sd))

-- SUBTAREAS

-- V1 O(n^2) siendo n la longitud de xs. Por cada elemento de xs hago addS O(n) sobre la misma lista.
-- V2 O(n) siendo n la longitud de xs. Por cada elemento de xs hago addS O(1).
listToSet :: Eq a => [a] -> Set a
listToSet [] = emptyS
listToSet (x:xs) = addS x (listToSet xs)

singularSi :: a -> Bool -> [a] -- O(1)
singularSi x True = [x]
singularSi _ _ = []