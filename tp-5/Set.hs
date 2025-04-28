module Set(Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where
-- No tiene repetidos y guarda en la estructura la cantidad de elementos.

data Set a = S [a] Int
-- Inv. Rep.
-- Sea (S xs c), un set, xs los elementos del mismo y c, la cantidad de elementos.
-- * xs no puede contener elementos repetidos.
-- * c es la cantidad de elementos de xs.

emptyS :: Set a -- O(1)
emptyS = S [] 0

addS :: Eq a => a -> Set a -> Set a -- O(n) siendo n la longitud de xs.
addS x (S xs c) = if elem x xs
                   then S xs c
                   else S (x:xs) (c+1)

belongs :: Eq a => a -> Set a -> Bool -- O(n) siendo n la longitud de xs.
belongs x (S xs c) = elem x xs

sizeS :: Eq a => Set a -> Int -- O(1)
sizeS (S _ c) = c

removeS :: Eq a => a -> Set a -> Set a -- O(n) siendo n la longitud de xs. O(n) + O(n) = O(n)
removeS x (S xs c) = if elem x xs
                      then S (remove x xs) (c-1)
                      else S xs c

unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs c) (S ys c2) = let newList = union xs ys
                             in S newList (length newList) 

setToList :: Eq a => Set a -> [a] -- O(1)
setToList (S xs c) = xs

-- SUBTAREAS

union :: Eq a => [a] -> [a] -> [a] -- O(n*m) siendo n la longitud de ys y m la longitud de xs. Por cada elemento de xs se recorre ys. 
union [] ys = ys
union (x:xs) ys = if elem x ys
                    then union xs ys
                    else x : (union xs ys)

remove :: Eq a => a -> [a] -> [a] -- O(n) sendo n la longitud de ys.
remove _ [] = []
remove x (y:ys) = if x==y
                    then ys
                    else y:(remove x ys)