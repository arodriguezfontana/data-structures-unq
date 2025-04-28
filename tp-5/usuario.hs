import Set

-- USUARIO SET

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _ = []
losQuePertenecen (x:xs) s = if belongs x s
                              then x : (losQuePertenecen xs s)
                              else losQuePertenecen xs s

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (listToSetSinRepetidos xs)

listToSetSinRepetidos :: Eq a => [a] -> Set a
listToSetSinRepetidos [] = emptyS
listToSetSinRepetidos (x:xs) = addS x (listToSetSinRepetidos xs)

unirTodos :: Eq a => Tree (Set a) -> Set a
