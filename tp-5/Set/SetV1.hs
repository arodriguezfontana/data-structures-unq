module SetV1(Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where

data Set a = S [a] Int
-- Inv. Rep.
-- Sea (S xs c) un set, xs una lista con sus elementos y c un numero.
-- * xs no puede contener elementos repetidos.
-- * c es la cantidad de elementos de xs.

emptyS :: Set a 
emptyS = S [] 0

addS :: Eq a => a -> Set a -> Set a 
addS x (S xs c) = if elem x xs
                   then S xs c
                   else S (x:xs) (c+1)

belongs :: Eq a => a -> Set a -> Bool 
belongs x (S xs c) = elem x xs

sizeS :: Eq a => Set a -> Int 
sizeS (S _ c) = c

removeS :: Eq a => a -> Set a -> Set a 
removeS x (S xs c) = if elem x xs
                      then S (remove x xs) (c-1)
                      else S xs c

unionS :: Eq a => Set a -> Set a -> Set a -- O(n*m)
unionS (S xs c) (S ys c2) = let newList = union xs ys
                             in S newList (length newList) 

setToList :: Eq a => Set a -> [a] 
setToList (S xs c) = xs

remove :: Eq a => a -> [a] -> [a] 
remove _ [] = []
remove x (y:ys) = if x==y
                    then ys
                    else y : (remove x ys)

union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys
union (x:xs) ys = if elem x ys
                    then union xs ys
                    else x : (union xs ys)