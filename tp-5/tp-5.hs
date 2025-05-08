-- O(1)
head' :: [a] -> a 
head' (x:xs) = x

-- O(1)
sumar :: Int -> Int 
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

-- O(n) siendo n el numero dado.
factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n-1)

-- O(n) siendo n la longitud de la xs.
longitud :: [a] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- O(n*m) siendo n la longitud de xs y m el numero dado. Por cada elemento de la lisata se realiza la funcion "factorial" O(n) sobre el numero dado.
factoriales :: [Int] -> [Int] 
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

-- O(n) siendo n la longitud de xs.
pertenece :: Eq a => a -> [a] -> Bool 
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

-- O(n^2) siendo n la longitud de xs. Por cada elemento de la lista se realiza la funcion "pertenece" O(n) sobre la misma lista.
sinRepetidos :: Eq a => [a] -> [a] 
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                        then sinRepetidos xs
                        else x : sinRepetidos xs

-- O(n) siendo n la longitud de xs.
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- O(n) siendo n la longitud de xs.
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

-- O(n) siendo n el menor entre el numero dado y la longitud de xs.
takeN :: Int -> [a] -> [a] 
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

-- O(n) siendo n el menor entre el numero dado y la longitud de xs.
dropN :: Int -> [a] -> [a] 
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

-- O(n) siendo n el menor entre el numero dado y la longitud de xs.
partir :: Int -> [a] -> ([a], [a]) 
partir n xs = (takeN n xs, dropN n xs)

-- O(n) siendo n la longitud de xs.
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

-- O(n) siendo n la longitud de xs.
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs

-- O(n^2) siendo n la longitud de xs. Por cada elemento de xs se realiza la funcion "minimo" O(n) sobre la misma lista.
ordenar :: Ord a => [a] -> [a] 
ordenar [] = []
orderar xs = let m = minimo xs
                in m : ordenar (sacar m xs)