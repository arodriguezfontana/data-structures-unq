head' :: [a] -> a -- O(1)
head' (x:xs) = x

sumar :: Int -> Int -- O(1)
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

factorial :: Int -> Int -- O(n) siendo n el numero dado.
factorial 0 = 1
factorial n = n * factorial (n-1)

longitud :: [a] -> Int -- O(n) siendo n la longitud de la xs.
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

factoriales :: [Int] -> [Int] -- O(n*m) siendo n la longitud de xs y m el numero dado. Por cada elemento de la lisata se realiza la funcion "factorial" O(n) sobre el numero dado.
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

pertenece :: Eq a => a -> [a] -> Bool -- O(n) siendo n la longitud de xs.
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

sinRepetidos :: Eq a => [a] -> [a] -- O(n^2) siendo n la longitud de xs. Por cada elemento de la lista se realiza la funcion "pertenece" O(n) sobre la misma lista.
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                        then sinRepetidos xs
                        else x : sinRepetidos xs

append :: [a] -> [a] -> [a] -- O(n) siendo n la longitud de xs.
append [] ys = ys
append (x:xs) ys = x : append xs ys

concatenar :: [String] -> String -- O(n) siendo n la longitud de xs.
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

takeN :: Int -> [a] -> [a] -- O(n) siendo n el menor entre el numero dado y la longitud de xs.
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

dropN :: Int -> [a] -> [a] -- O(n) siendo n el menor entre el numero dado y la longitud de xs.
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

partir :: Int -> [a] -> ([a], [a]) -- O(n) siendo n el menor entre el numero dado y la longitud de xs. O(n) + O(n) = O(n).
partir n xs = (takeN n xs, dropN n xs)

minimo :: Ord a => [a] -> a -- O(n) siendo n la longitud de xs.
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

sacar :: Eq a => a -> [a] -> [a] -- O(n) siendo n la longitud de xs.
sacar n [] = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs

ordenar :: Ord a => [a] -> [a] -- O(n^2) siendo n la longitud de xs. Por cada elemento de xs se realiza la funcion "minimo" O(n) sobre la misma lista.
ordenar [] = []
orderar xs = let m = minimo xs
                in m : ordenar (sacar m xs)