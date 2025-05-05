module SetV1(Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where

data Set a = S [a] Int
-- Inv. Rep.
-- Sea (S xs c) un set, xs los elementos del mismo y c la cantidad de elementos.
-- * xs no puede contener elementos repetidos.
-- * c es la cantidad de elementos de xs.

emptyS :: Set a -- O(1)
-- Crea un conjunto vacío.
emptyS = S [] 0

addS :: Eq a => a -> Set a -> Set a -- O(n) siendo n la longitud de xs.
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (S xs c) = if elem x xs
                   then S xs c
                   else S (x:xs) (c+1)

belongs :: Eq a => a -> Set a -> Bool -- O(n) siendo n la longitud de xs.
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (S xs c) = elem x xs

sizeS :: Eq a => Set a -> Int -- O(1)
-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S _ c) = c

removeS :: Eq a => a -> Set a -> Set a -- O(n) siendo n la longitud de xs. O(n) + O(n) = O(n).
-- Borra un elemento del conjunto.
removeS x (S xs c) = if elem x xs
                      then S (remove x xs) (c-1)
                      else S xs c

unionS :: Eq a => Set a -> Set a -> Set a -- O(n*m) siendo n la longitud de ys y m la longitud de xs. O(n*m) + O(n*m) = O(n*m).
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS (S xs c) (S ys c2) = let newList = union xs ys
                             in S newList (length newList) 

setToList :: Eq a => Set a -> [a] -- O(1)
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S xs c) = xs

-- SUBTAREAS

remove :: Eq a => a -> [a] -> [a] -- O(n) sendo n la longitud de ys.
remove _ [] = []
remove x (y:ys) = if x==y
                    then remove x ys
                    else y : (remove x ys)

union :: Eq a => [a] -> [a] -> [a] -- O(n*m) siendo n la longitud de ys y m la longitud de xs. Por cada elemento de xs se recorre ys. 
union [] ys = ys
union (x:xs) ys = if elem x ys
                    then union xs ys
                    else x : (union xs ys)