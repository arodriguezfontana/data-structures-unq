module SetV2(Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where

data Set a = S [a]
-- Inv. Rep.
-- Sea (S xs), un set y xs una lista con sus elementos.
-- * No hay invariantes.

emptyS :: Set a 
emptyS = S []

addS :: Eq a => a -> Set a -> Set a 
addS x (S xs) = S (x:xs)

belongs :: Eq a => a -> Set a -> Bool 
belongs x (S xs) = elem x xs

sizeS :: Eq a => Set a -> Int 
sizeS (S xs) = length (sinRepetidos xs)

removeS :: Eq a => a -> Set a -> Set a 
removeS x (S xs) = if elem x xs
                      then S (remove x xs)
                      else S xs

unionS :: Eq a => Set a -> Set a -> Set a 
unionS (S xs) (S ys) = S (xs++ys)

setToList :: Eq a => Set a -> [a] 
setToList (S xs) = sinRepetidos xs

remove :: Eq a => a -> [a] -> [a] 
remove _ [] = []
remove x (y:ys) = if x==y
                    then remove x ys
                    else y : (remove x ys)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) = if elem x xs
                        then sinRepetidos xs
                        else x : sinRepetidos xs