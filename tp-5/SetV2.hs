module SetV2(SetV2, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where
-- Admite repetidos y guarda en la estructura la cantidad de elementos.

data Set a = S [a]
-- Inv. Rep.
-- Sea (S xs), un set y xs los elementos del mismo.
-- * No hay invariantes.

emptyS :: Set a -- O(1)
-- Crea un conjunto vacío.
emptyS = S []

addS :: Eq a => a -> Set a -> Set a -- O(1)
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (S xs) = S (x:xs)

belongs :: Eq a => a -> Set a -> Bool -- O(n) siendo n la longitud de xs.
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (S xs) = elem x xs

sizeS :: Eq a => Set a -> Int -- O(n) siendo n la longitud de xs.
-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S xs) = length (sinRepetidos xs)

removeS :: Eq a => a -> Set a -> Set a -- O(n) siendo n la longitud de xs. O(n) + O(n) = O(n)
-- Borra un elemento del conjunto.
removeS x (S xs) = if elem x xs
                      then S (remove x xs)
                      else S xs

unionS :: Eq a => Set a -> Set a -> Set a -- O(1)
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS (S xs c) (S ys c2) = S (xs++ys)

setToList :: Eq a => Set a -> [a] -- O(1)
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S xs c) = sinRepetidos xs

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