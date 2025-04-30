-- PIZZAS

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

cantidadDeCapas :: Pizza -> Int
-- Dada una pizza devuelve la cantidad de ingredientes.
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

armarPizza :: [Ingrediente] -> Pizza
-- Dada una lista de ingredientes construye una pizza.
armarPizza [] = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

sacarJamon :: Pizza -> Pizza
-- Le saca los ingredientes que sean jamón a la pizza.
sacarJamon Prepizza = Prepizza
sacarJamon (Capa i p) = if sonMismoIngrediente Jamon i
                         then sacarJamon p
                         else Capa i (sacarJamon p)

sonMismoIngrediente :: Ingrediente -> Ingrediente -> Bool
sonMismoIngrediente Salsa Salsa = True
sonMismoIngrediente Queso Queso = True
sonMismoIngrediente Jamon Jamon = True
sonMismoIngrediente (Aceitunas _) (Aceitunas _) = True
sonMismoIngrediente _ _ = False

tieneSoloSalsaYQueso :: Pizza -> Bool
-- Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero).
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa i p) = esSalsaOQueso i && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _ = False

duplicarAceitunas :: Pizza -> Pizza
-- Recorre cada ingrediente y si es aceitunas duplica su cantidad.
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = if sonAceitunas i
                                then Capa (aceitunasDuplicadas i) (duplicarAceitunas p)
                                else Capa i (duplicarAceitunas p)

sonAceitunas :: Ingrediente -> Bool
sonAceitunas (Aceitunas _) = True
sonAceitunas _ = False

aceitunasDuplicadas :: Ingrediente -> Ingrediente
aceitunasDuplicadas (Aceitunas n) = Aceitunas (n*2)

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
-- Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : (cantCapasPorPizza ps)

-- MAPA DE TESOROS CON BIFURCACIONES

data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show

hayTesoro :: Mapa -> Bool
-- Indica si hay un tesoro en alguna parte del mapa.
hayTesoro (Fin c) = hayTesoroEnCofre c
hayTesoro (Bifurcacion c mi md) =  hayTesoroEnCofre c || (hayTesoro mi) || (hayTesoro md)

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre os) = hayTesoroEnObjetos os

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos [] = False
hayTesoroEnObjetos (o:os) = esTesoro o || hayTesoroEnObjetos os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

hayTesoroEn :: [Dir] -> Mapa -> Bool
-- Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una lista vacía de direcciones.
hayTesoroEn [] m = hayTesoroAca m
hayTesoroEn (d:ds) m = puedoAvanzar m && hayTesoroEn ds (mapaAvanzado d m)
    
puedoAvanzar :: Mapa -> Bool
puedoAvanzar (Fin _) = False
puedoAvanzar _ = True

hayTesoroAca :: Mapa -> Bool
hayTesoroAca (Fin c) = hayTesoroEnCofre c
hayTesoroAca (Bifurcacion c _ _) = hayTesoroEnCofre c

mapaAvanzado :: Dir -> Mapa -> Mapa
mapaAvanzado _ (Fin _) = error "No se puede avanzar."
mapaAvanzado d (Bifurcacion _ mi md) = if esIzq d
                                        then mi
                                        else md

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _ = False

caminoAlTesoro :: Mapa -> [Dir]
-- Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c mi md) = if hayTesoroEnCofre c 
                                        then []
                                        else if hayTesoro mi
                                             then Izq : caminoAlTesoro mi
                                             else Der : caminoAlTesoro md

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion c mi md) = if esRamaMasLarga mi md
                                                then Izq : caminoDeLaRamaMasLarga mi
                                                else Der : caminoDeLaRamaMasLarga md 

esRamaMasLarga :: Mapa -> Mapa -> Bool
esRamaMasLarga m1 m2 = longitudCamino m1 > longitudCamino m2

longitudCamino :: Mapa -> Int
longitudCamino (Fin _) = 0
longitudCamino (Bifurcacion _ m1 m2) = 1 + max (longitudCamino m1) (longitudCamino m2)

tesorosPorNivel :: Mapa -> [[Objeto]]
-- Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel (Fin c) = [tesorosDe c]
tesorosPorNivel (Bifurcacion c mi md) = tesorosDe c : zipListaDeListas (tesorosPorNivel mi) (tesorosPorNivel md)

zipListaDeListas :: [[a]] -> [[a]] -> [[a]]
zipListaDeListas [] yss = yss
zipListaDeListas xss [] = xss
zipListaDeListas (xs:xss) (ys:yss) = (ys ++ xs) : (zipListaDeListas xss yss)

tesorosDe :: Cofre -> [Objeto]
tesorosDe (Cofre os) = tesorosEn os

tesorosEn :: [Objeto] -> [Objeto]
tesorosEn [] = []
tesorosEn (o:os) = singularSi o (esTesoro o) ++ tesorosEn os

singularSi :: a -> Bool -> [a]
singularSi x True = [x]
singularSi _ _ = []

todosLosCaminos :: Mapa -> [[Dir]]
-- Devuelve todos lo caminos en el mapa.
todosLosCaminos (Fin _) = [[]]
todosLosCaminos (Bifurcacion _ mi md) = agregoACada Izq (todosLosCaminos mi) ++ agregoACada Der (todosLosCaminos md)

agregoACada :: a -> [[a]] -> [[a]]
agregoACada _ [] = []
agregoACada x (ys:yss) = (x : ys) : agregoACada x yss

-- NAVE ESPACIAL

type SectorId = String
type Tripulante = String
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Sector = S SectorId [Componente] [Tripulante] deriving Show
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show

sectores :: Nave -> [SectorId]
-- Propósito: Devuelve todos los sectores de la nave.
sectores (N ts) = sectoresT ts

sectoresT :: Tree Sector -> [SectorId]
sectoresT EmptyT = []
sectoresT (NodeT s si sd) = sectorIdDe s : (sectoresT si) ++ (sectoresT sd)

sectorIdDe :: Sector -> SectorId
sectorIdDe (S sid _ _) = sid

poderDePropulsion :: Nave -> Int
-- Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota: el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion (N ts) = poderDePropulsionT ts

poderDePropulsionT :: Tree Sector -> Int
poderDePropulsionT EmptyT = 0
poderDePropulsionT (NodeT s si sd) = poderDePropulsionS s + (poderDePropulsionT si) + (poderDePropulsionT sd)

poderDePropulsionS :: Sector -> Int
poderDePropulsionS (S _ cs _) = poderDePropulsionCS cs

poderDePropulsionCS :: [Componente] -> Int
poderDePropulsionCS [] = 0
poderDePropulsionCS (c:cs) = poderDePropulsionSiEsMotor c + poderDePropulsionCS cs

poderDePropulsionSiEsMotor :: Componente -> Int
poderDePropulsionSiEsMotor (Motor i) = i
poderDePropulsionSiEsMotor _ = 0

barriles :: Nave -> [Barril]
-- Propósito: Devuelve todos los barriles de la nave.
barriles (N ts) = barrilesT ts

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT = []
barrilesT (NodeT s si sd) = barrilesS s ++ (barrilesT si) ++ (barrilesT sd)

barrilesS :: Sector -> [Barril]
barrilesS (S _ cs _) = barrilesCS cs

barrilesCS :: [Componente] -> [Barril]
barrilesCS [] = []
barrilesCS (c:cs) = barrilesSiEsAlmacen c ++ barrilesCS cs

barrilesSiEsAlmacen :: Componente -> [Barril]
barrilesSiEsAlmacen (Almacen bs) = bs
barrilesSiEsAlmacen _ = []

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- Propósito: Añade una lista de componentes a un sector de la nave. Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector cs sid (N ts) = N (tsConComponentes cs sid ts)

tsConComponentes :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
tsConComponentes cs sid (EmptyT) = EmptyT
tsConComponentes cs sid (NodeT s si sd) = NodeT (sConComponentesAgregadosSi cs (sid == (sectorIdDe s)) s) (tsConComponentes cs sid si) (tsConComponentes cs sid sd)

sConComponentesAgregadosSi :: [Componente] -> Bool -> Sector -> Sector
sConComponentesAgregadosSi cs True s = sConComponentesAgregados cs s
sConComponentesAgregadosSi cs _ s = s

sConComponentesAgregados :: [Componente] -> Sector -> Sector
sConComponentesAgregados cs (S sid c2s ts) = S sid (cs++c2s) ts

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
-- Propósito: Incorpora un tripulante a una lista de sectores de la nave. Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA t [] n = n
asignarTripulanteA t (sid:sids) n = naveConTAgregadoEnS t sid (asignarTripulanteA t sids n)

naveConTAgregadoEnS :: Tripulante -> SectorId -> Nave -> Nave
naveConTAgregadoEnS t sid (N ts) = N (tsConTAgregadoEnST t sid ts)

tsConTAgregadoEnST :: Tripulante -> SectorId -> Tree Sector -> Tree Sector
tsConTAgregadoEnST t s EmptyT = EmptyT
tsConTAgregadoEnST t sid (NodeT s si sd) = NodeT (sConTripulanteAgregadoSi t (sid == sectorIdDe s) s) (tsConTAgregadoEnST t sid si) (tsConTAgregadoEnST t sid sd)

sConTripulanteAgregadoSi :: Tripulante -> Bool -> Sector -> Sector
sConTripulanteAgregadoSi t True (S sid cs ts) = S sid cs (t:ts)
sConTripulanteAgregadoSi _ _ s = s

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
-- Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados t (N ts) = sectoresAsignadosT t ts

sectoresAsignadosT :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosT t EmptyT = []
sectoresAsignadosT t (NodeT s si sd) = sectorSiAparece t s ++ (sectoresAsignadosT t si) ++ (sectoresAsignadosT t sd)

sectorSiAparece :: Tripulante -> Sector -> [SectorId]
sectorSiAparece t (S sid cs ts) = if elem t ts
                                   then [sid]
                                   else []

tripulantes :: Nave -> [Tripulante]
-- Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes (N ts) = sinRepetidos (tripulanteT ts)

tripulanteT :: Tree Sector -> [Tripulante]
tripulanteT EmptyT = []
tripulanteT (NodeT s si sd) = tripulantesDe s ++ tripulanteT si ++ tripulanteT sd

tripulantesDe :: Sector -> [Tripulante]
tripulantesDe (S _ _ ts) = ts

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if elem x xs then sinRepetidos xs else x : sinRepetidos xs

-- MANADA DE LOBOS

type Presa = String 
type Territorio = String 
type Nombre = String 
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre deriving Show
data Manada = M Lobo deriving Show

buenaCaza :: Manada -> Bool
-- Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza (M l) = cantPresas l > cantCrias l

cantPresas :: Lobo -> Int
cantPresas (Cria _) = 0
cantPresas (Explorador _ _ l1 l2) = cantPresas l1 + cantPresas l2
cantPresas (Cazador _ ps l1 l2 l3) = longitud ps + cantPresas l1 + cantPresas l2 + cantPresas l3 

cantCrias :: Lobo -> Int
cantCrias (Cria _) = 1
cantCrias (Explorador _ _ l1 l2) = cantCrias l1 + cantCrias l2
cantCrias (Cazador _ _ l1 l2 l3) = cantCrias l1 + cantCrias l2 + cantCrias l3

esCria :: Lobo -> Bool
esCria (Cria _) = True
esCria _ = False

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

elAlfa :: Manada -> (Nombre, Int)
-- Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de cero presas.
elAlfa (M l) = elAlfaL l

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cria n) = (n, 0)
elAlfaL (Explorador n _ l1 l2) = elAlfaLS [elAlfaL l1, elAlfaL l2, (n, 0)]
elAlfaL (Cazador n ps l1 l2 l3) = elAlfaLS [(n, longitud ps), elAlfaL l1, elAlfaL l2, leAlfaL l3]

elAlfaLS :: [(Nombre, Int)] -> (Nombre, Int)
elAlfaLS (ni:[]) = nc
elAlfaLS (ni: nis) = elMásAlfa ni (elAlfaLS nis)

elMásAlfa :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elMásAlfa (n1, i1) (n2, i2) = if i1 > i2
                                then (n1, i1)
                                else (n2, i2)

losQueExploraron :: Territorio -> Manada -> [Nombre]
-- Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que pasaron por dicho territorio.
losQueExploraron t (M l) = losQueExploraronL t l

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cazador _ _ l1 l2 l3) = losQueExploraronL t l1 ++ losQueExploraronL t l2 ++ losQueExploraronL t l3
losQueExploraronL t (Explorador n ts l1 l2) = singularSi n (elem t ts) ++ losQueExploraronL t l1 ++ losQueExploraronL t l2
losQueExploraronL _ (Cria _) = []

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
-- Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron dicho territorio. Los territorios no deben repetirse.
exploradoresPorTerritorio m = asociarPorTerritorio (territorios m) m

asociarPorTerritorio :: [Territorio] -> Manada -> -> [(Territorio, [Nombre])]
asociarPorTerritorio [] _ = []
asociarPorTerritorio (t:ts) m = (t, losQueExploraron t m) : asociarPorTerritorio ts m

territorios :: Manada -> [Territorio]
territorios (M l) = sinRepetidos (territoriosL l)

territoriosL :: Lobo -> [Territorio]
territoriosL (Cria _) = []
territoriosL (Explorador n ts l1 l2) = ts ++ territoriosL l1 ++ territoriosL l2
territoriosL (Cazador n ps l1 l2 l3) = territoriosL l1 ++ territoriosL l2 ++ territoriosL l3

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if elem x xs then sinRepetidos xs else x : sinRepetidos xs

-- cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
-- -- Propósito: dado el nombre de un lobo y una manada, indica el nombre de todos los cazadores que tienen como subordinado al lobo dado (puede ser un subordinado directo, o el subordinado de un subordinado). Precondición: hay un lobo con dicho nombre y es único.
