import SetV1

-- USUARIO SET

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen [] _ = []
losQuePertenecen (x:xs) s = if belongs x s
                              then x : (losQuePertenecen xs s)
                              else losQuePertenecen xs s

sinRepetidos :: Eq a => [a] -> [a]
-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos xs = setToList (listToSet xs)

listToSet :: Eq a => [a] -> Set a
listToSet [] = emptyS
listToSet (x:xs) = addS x (listToSet xs)

unirTodos :: Eq a => Tree (Set a) -> Set a
-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
unirTodos EmptyT = emptyS
unirTodos (NodeT s si sd) = unionS s (unionS (unirTodos si) (unirTodos sd)) 