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

