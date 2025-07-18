data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

nroBolitas :: Color -> Celda -> Int
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
poner c CeldaVacia = Bolita c CeldaVacia
poner c (Bolita cb cl) = Bolita c (Bolita cb cl)

sacar :: Color -> Celda -> Celda
sacar c CeldaVacia = CeldaVacia
sacar c (Bolita cb cl) = if sonMismoColor c cb
                          then cl
                          else Bolita cb (sacar c cl)

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ cl = cl
ponerN n c cl = poner c (ponerN (n-1) c cl)

data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show

hayTesoro :: Camino -> Bool
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
pasosHastaTesoro Fin = error "No hay camino que recorrer."
pasosHastaTesoro (Cofre os c) = if hayTesoroEnObjetos os
                                 then 0
                                 else 1 + pasosHastaTesoro c
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 (Cofre os _) = hayTesoroEnObjetos os
hayTesoroEn 0 (Nada _) = False
hayTesoroEn _ Fin = False
hayTesoroEn n (Cofre _ c) = hayTesoroEn (n-1) c
hayTesoroEn n (Nada c) = hayTesoroEn (n-1) c

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n c = cantTesorosEnCamino c >= n

cantTesorosEnCamino :: Camino -> Int
cantTesorosEnCamino Fin = 0
cantTesorosEnCamino (Cofre os c) = cantTesorosEnObjetos os + cantTesorosEnCamino c
cantTesorosEnCamino (Nada c) = 0 + cantTesorosEnCamino c

cantTesorosEnObjetos :: [Objeto] -> Int
cantTesorosEnObjetos [] = 0
cantTesorosEnObjetos (o:os) = unoSi (esTesoro o) + cantTesorosEnObjetos os

cantTesorosEntre :: Int -> Int -> Camino -> Int
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

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + (sumarT t1) + (sumarT t2)

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT n ri rd) = 1 + (sizeT ri) + (sizeT rd)

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n ri rd) = NodeT (n*2) (mapDobleT ri) (mapDobleT rd)

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT x (NodeT n ri rd) = x==n || (perteneceT x ri) || (perteneceT x rd)

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT x (NodeT n ri rd) = unoSi (x==n) + (aparicionesT x ri) + (aparicionesT x rd)

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT n EmptyT EmptyT) = [n]
leaves (NodeT _ ri rd) = leaves ri ++ leaves rd

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT n ri rd) = 1 + max (heightT ri) (heightT rd)

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT n ri rd) = NodeT n (mirrorT rd) (mirrorT ri)

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT n ri rd) = toList ri ++ [n] ++ toList rd

levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT n ri rd) = [n]
levelN m (NodeT n ri rd) = levelN (m-1) ri ++ levelN (m-1) rd

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT n ri rd) = [n] : zipListaDeListas (listPerLevel ri) (listPerLevel rd)

zipListaDeListas :: [[a]] -> [[a]] -> [[a]]
zipListaDeListas [] yss = yss
zipListaDeListas xss [] = xss
zipListaDeListas (xs:xss) (ys:yss) = (ys ++ xs) : (zipListaDeListas xss yss)

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT n ri rd) = n : laDeMayorLongitud (ramaMasLarga ri) (ramaMasLarga rd)

laDeMayorLongitud :: [a] -> [a] -> [a]
laDeMayorLongitud xs ys = if longitud xs > longitud ys
                           then xs
                           else ys

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT n ri rd) = [n] : (agregoACada n (todosLosCaminos ri) ++ agregoACada n (todosLosCaminos rd))

agregoACada :: a -> [[a]] -> [[a]]
agregoACada _ [] = []
agregoACada x (ys:yss) = (x : ys) : agregoACada x yss

todosLosCaminosMaximales :: Tree a -> [[a]]
todosLosCaminosMaximales EmptyT = []
todosLosCaminosMaximales (NodeT n EmptyT EmptyT) = [[n]]
todosLosCaminosMaximales (NodeT n ri rd) = (agregoACada n (todosLosCaminosMaximales ri)) ++ (agregoACada n (todosLosCaminosMaximales rd))

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA

eval :: ExpA -> Int
eval (Valor i) = i
eval (Sum e1 e2) = eval e1 + eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval (Neg e) = - eval e

simplificar :: ExpA -> ExpA
simplificar (Valor i) = Valor i
simplificar (Sum e1 e2) = simplificarSum (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2) = simplificarProd (simplificar e1) (simplificar e2)
simplificar (Neg e) = simplificarNeg (simplificar e)

simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum (Valor 0) e = e
simplificarSum e (Valor 0) = e
simplificarSum e1 e2 = Sum e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0) e = Valor 0
simplificarProd e (Valor 0) = Valor 0
simplificarProd (Valor 1) e = e
simplificarProd e (Valor 1) = e
simplificarProd e1 e2 = Prod e1 e2

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg e) = e
simplificarNeg (Valor 0) = Valor 0
simplificarNeg e = Neg e