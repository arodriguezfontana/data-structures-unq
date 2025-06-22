module MultiSetV1(MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList) where

import MapV1

data MultiSet a = MS (Map a Int)
-- Sea (M mp) un multiset y mp un map:

emptyMS :: MultiSet a
-- Propósito: denota un multiconjunto vacío.
emptyMS = MS emptyM

addMS :: Ord a => a -> MultiSet a -> MultiSet a
-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
addMS x (MS mp) = case lookupM x mp of
                    Nothing <- MS (assocM x 1 mp)
                    Just v <- MS (assocM x (v+1) mp)

ocurrencesMS :: Ord a => a -> MultiSet a -> Int 
-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
ocurrencesMS x (MS mp) = case lookupM x mp of
                            Nothing <- 0
                            Just v <- v

unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
-- Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos.
unionMS (MS mp1) (MS mp2) = 

intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
-- Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos multiconjuntos tienen en común.
intersectionMS (MS mp1) (MS mp2) =

multiSetToList :: MultiSet a -> [(a, Int)]
-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su cantidad de ocurrencias.
multiSetToList (MS mp) = mapToList (keys mp) mp

-- SUBTAREAS

mapToList :: [k] -> Map k Int -> [(k, Int)]
-- Precondición: Todas las claves son las del mp dado.
mapToList [] _ = [] 
mapToList (k:ks) mp = (k, fromJust(lookupM k mp)) : mapToList ks mp