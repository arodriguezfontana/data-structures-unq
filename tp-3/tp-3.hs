-- TIPOS RECURSIVOS SIMPLES

data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

nroBolitas :: Color -> Celda -> Int
-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya existe una operación sobre listas que ayude a resolver el problema.
nroBolitas _ CeldaVacia = 0
nroBolitas c (Bolita cb cl) = unoSi (sonMismoColor c cb) + nroBolitas c cl

sonMismoColor :: Color -> Color -> Bool
sonMismoColor Azul Azul = True
sonMismoColor Rojo Rojo = True
sonMismoColor _ _ = False

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

poner :: Color -> Celda -> Celda
-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner c CeldaVacia = Bolita c CeldaVacia
poner c (Bolita cb cl) = Bolita c (Bolita cb cl)

sacar :: Color -> Celda -> Celda
-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de Gobstones, esta función es total.
sacar c CeldaVacia = CeldaVacia
sacar c (Bolita cb cl) = if sonMismoColor c cb
                          then cl
                          else Bolita cb (sacar c cl)

ponerN :: Int -> Color -> Celda -> Celda
-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN 0 _ cl = cl
ponerN n c cl = poner c (ponerN (n-1) c cl)

data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show

hayTesoro :: Camino -> Bool
-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro Fin = False
hayTesoro (Cofre os c) = hayTesoroEnObjetos os || hayTesoro c
hayTesoro (Nada c) = hayTesoro c

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos [] = False
hayTesoroEnObjetos (o:os) = esTesoro o || hayTesoroEnObjetos os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

pasosHastaTesoro :: Camino -> Int
-- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro. Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0. Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro Fin = error "No hay camino que recorrer."
pasosHastaTesoro (Cofre os c) = if hayTesoroEnObjetos os
                                 then 0
                                 else 1 + pasosHastaTesoro c
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c

hayTesoroEn :: Int -> Camino -> Bool
-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn 0 (Cofre os _) = hayTesoroEnObjetos os
hayTesoroEn 0 (Nada _) = False
hayTesoroEn _ Fin = False
hayTesoroEn n (Cofre _ c) = hayTesoroEn (n-1) c
hayTesoroEn n (Nada c) = hayTesoroEn (n-1) c

alMenosNTesoros :: Int -> Camino -> Bool
-- Indica si hay al menos n tesoros en el camino.
alMenosNTesoros n c = cantTesorosEnCamino c >= n

cantTesorosEnCamino :: Camino -> Int
cantTesorosEnCamino Fin = 0
cantTesorosEnCamino (Cofre os c) = cantTesorosEnObjetos os + cantTesorosEnCamino c
cantTesorosEnCamino (Nada c) = 0 + cantTesorosEnCamino c

cantTesorosEnObjetos :: [Objeto] -> Int
cantTesorosEnObjetos [] = 0
cantTesorosEnObjetos (o:os) = unoSi (esTesoro o) + cantTesorosEnObjetos os

cantTesorosEntre :: Int -> Int -> Camino -> Int
-- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre n m c = cantTesorosEnCamino (caminoHasta m (caminoDesde n c))

caminoDesde :: Int -> Camino -> Camino
caminoDesde 0 c = c
caminoDesde _ Fin = error "No hay camino que recorrer."
caminoDesde n (Cofre os c) = caminoDesde (n-1) c
caminoDesde n (Nada c) = caminoDesde (n-1) c

caminoHasta :: Int -> Camino -> Camino
caminoHasta 0 _ = Fin
caminoHasta _ Fin = Fin
caminoHasta n (Cofre os c) = Cofre os (caminoHasta (n-1) c)
caminoHasta n (Nada c) = Nada (caminoHasta (n-1) c)

-- TIPOS ARBÓREOS

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

t1 :: Tree Int
t1 = NodeT 4 (NodeT 5 (NodeT 6 EmptyT EmptyT) EmptyT) 
             (NodeT 3 EmptyT (NodeT 7 (NodeT 6 EmptyT EmptyT) EmptyT))

sumarT :: Tree Int -> Int
-- Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + (sumarT t1) + (sumarT t2)

sizeT :: Tree a -> Int
-- Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).
sizeT EmptyT = 0
sizeT (NodeT n ri rd) = 1 + (sizeT ri) + (sizeT rd)

mapDobleT :: Tree Int -> Tree Int
-- Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n ri rd) = NodeT (n*2) (mapDobleT ri) (mapDobleT rd)

perteneceT :: Eq a => a -> Tree a -> Bool
-- Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
perteneceT _ EmptyT = False
perteneceT x (NodeT n ri rd) = x==n || (perteneceT x ri) || (perteneceT x rd)

aparicionesT :: Eq a => a -> Tree a -> Int
-- Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
aparicionesT _ EmptyT = 0
aparicionesT x (NodeT n ri rd) = unoSi (x==n) + (aparicionesT x ri) + (aparicionesT x rd)

leaves :: Tree a -> [a]
-- Dado un árbol devuelve los elementos que se encuentran en sus hojas. NOTA: en este tipo se define como hoja a un nodo con dos hijos vacíos.
leaves EmptyT = []
leaves (NodeT n EmptyT EmptyT) = [n]
leaves (NodeT _ ri rd) = leaves ri ++ leaves rd

heightT :: Tree a -> Int
-- Dado un árbol devuelve su altura. Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad de niveles del árbol. La altura para EmptyT es 0, y para una hoja es 1.
heightT EmptyT = 0
heightT (NodeT n ri rd) = 1 + max (heightT ri) (heightT rd)

mirrorT :: Tree a -> Tree a
-- Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del árbol.
mirrorT EmptyT = EmptyT
mirrorT (NodeT n ri rd) = NodeT n (mirrorT rd) (mirrorT ri)

toList :: Tree a -> [a]
-- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order. Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz y luego los elementos del hijo derecho.
toList EmptyT = []
toList (NodeT n ri rd) = toList ri ++ [n] ++ toList rd

-- levelN :: Int -> Tree a -> [a]
-- -- Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1. Nota: El primer nivel de un árbol (su raíz) es 0.
-- levelN m EmptyT =
-- levelN m (NodeT n ri rd) = (levelN m ri) (levelN m rd)

-- listPerLevel :: Tree a -> [[a]]
-- -- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.

ramaMasLarga :: Tree a -> [a]
-- Devuelve los elementos de la rama más larga del árbol
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT n ri rd) = n : laDeMayorLongitud (ramaMasLarga ri) (ramaMasLarga rd)

laDeMayorLongitud :: [a] -> [a] -> [a]
laDeMayorLongitud xs ys = if longitud xs > longitud ys
                           then xs
                           else ys

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- todosLosCaminos :: Tree a -> [[a]]
-- -- Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz hasta cualquiera de los nodos. ATENCIÓN: se trata de todos los caminos, y no solamente de los maximales (o sea, de la raíz hasta la hoja).

-- data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA

-- eval :: ExpA -> Int
-- -- Dada una expresión aritmética devuelve el resultado evaluarla.

-- simplificar :: ExpA -> ExpA
-- -- Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando notación matemática convencional):
-- -- a) 0 + x = x + 0 = x
-- -- b) 0 * x = x * 0 = 0
-- -- c) 1 * x = x * 1 = x
-- -- d) - (- x) = x