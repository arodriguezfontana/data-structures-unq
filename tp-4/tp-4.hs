data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

sacarJamon :: Pizza -> Pizza
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
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa i p) = esSalsaOQueso i && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _ = False

duplicarAceitunas :: Pizza -> Pizza
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
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : (cantCapasPorPizza ps)

data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show

hayTesoro :: Mapa -> Bool
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
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c mi md) = if hayTesoroEnCofre c 
                                        then []
                                        else if hayTesoro mi
                                             then Izq : caminoAlTesoro mi
                                             else Der : caminoAlTesoro md

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
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
todosLosCaminos (Fin _) = [[]]
todosLosCaminos (Bifurcacion _ mi md) = agregoACada Izq (todosLosCaminos mi) ++ agregoACada Der (todosLosCaminos md)

agregoACada :: a -> [[a]] -> [[a]]
agregoACada _ [] = []
agregoACada x (ys:yss) = (x : ys) : agregoACada x yss

type SectorId = String
type Tripulante = String
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Sector = S SectorId [Componente] [Tripulante] deriving Show
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show

sectores :: Nave -> [SectorId]
sectores (N ts) = sectoresT ts

sectoresT :: Tree Sector -> [SectorId]
sectoresT EmptyT = []
sectoresT (NodeT s si sd) = sectorIdDe s : (sectoresT si) ++ (sectoresT sd)

sectorIdDe :: Sector -> SectorId
sectorIdDe (S sid _ _) = sid

poderDePropulsion :: Nave -> Int
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
sectoresAsignados t (N ts) = sectoresAsignadosT t ts

sectoresAsignadosT :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosT t EmptyT = []
sectoresAsignadosT t (NodeT s si sd) = sectorSiAparece t s ++ (sectoresAsignadosT t si) ++ (sectoresAsignadosT t sd)

sectorSiAparece :: Tripulante -> Sector -> [SectorId]
sectorSiAparece t (S sid cs ts) = if elem t ts
                                   then [sid]
                                   else []

tripulantes :: Nave -> [Tripulante]
tripulantes (N ts) = sinRepetidos (tripulanteT ts)

tripulanteT :: Tree Sector -> [Tripulante]
tripulanteT EmptyT = []
tripulanteT (NodeT s si sd) = tripulantesDe s ++ tripulanteT si ++ tripulanteT sd

tripulantesDe :: Sector -> [Tripulante]
tripulantesDe (S _ _ ts) = ts

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if elem x xs then sinRepetidos xs else x : sinRepetidos xs

type Presa = String 
type Territorio = String 
type Nombre = String 
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre deriving Show
data Manada = M Lobo deriving Show

buenaCaza :: Manada -> Bool
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
losQueExploraron t (M l) = losQueExploraronL t l

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL _ (Cria _) = []
losQueExploraronL t (Cazador _ _ l1 l2 l3) = losQueExploraronL t l1 ++ losQueExploraronL t l2 ++ losQueExploraronL t l3
losQueExploraronL t (Explorador n ts l1 l2) = singularSi n (elem t ts) ++ losQueExploraronL t l1 ++ losQueExploraronL t l2

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio m = asociarPorTerritorio (territorios m) m

asociarPorTerritorio :: [Territorio] -> Manada -> -> [(Territorio, [Nombre])]
asociarPorTerritorio [] _ = []
asociarPorTerritorio (t:ts) m = (t, losQueExploraron t m) : asociarPorTerritorio ts m

territorios :: Manada -> [Territorio]
territorios (M l) = sinRepetidos (territoriosL l)

territoriosL :: Lobo -> [Territorio]
territoriosL (Cria _) = []
territoriosL (Cazador n ps l1 l2 l3) = territoriosL l1 ++ territoriosL l2 ++ territoriosL l3
territoriosL (Explorador n ts l1 l2) = ts ++ territoriosL l1 ++ territoriosL l2

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if elem x xs then sinRepetidos xs else x : sinRepetidos xs

cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
cazadoresSuperioresDe n m = cConSubordinadoA n m

cConSubordinadoA :: Nombre -> Lobo -> [Nombre]
cConSubordinadoA n (Cria _) = []
cConSubordinadoA n (Explorador n2 ts l1 l2) = if n == n2
                                                  then []
                                                  else cConSubordinadoA n l1 ++ cConSubordinadoA n l2 
cConSubordinadoA n (Cazador n2 ps l1 l2 l3) = let c1 = cConSubordinadoA n l1
                                                  c2 = cConSubordinadoA n l2
                                                  c3 = cConSubordinadoA n l3
                                             in if n == n2
                                                  then []
                                                  else n2 : (c1 ++ c2 ++ c3)