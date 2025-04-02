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
-- Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.).
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

c1 = Cofre o1
c2 = Cofre o1
c3 = Cofre o2
o1 = [Chatarra, Chatarra]
o2 = [Chatarra, Tesoro]

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

m = Bifurcacion c1 (Bifurcacion c1 (Bifurcacion c2 (Fin c2) (Fin c1)) (Fin c1))
                   (Bifurcacion c2 (Fin c1) (Fin c3))

-- hayTesoroEn :: [Dir] -> Mapa -> Bool
-- -- Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una lista vacía de direcciones.
    
caminoAlTesoro :: Mapa -> [Dir]
-- Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c mi md) = if hayTesoroEnCofre c
                                        then []
                                        else if hayTesoro mi
                                              then Izq : (caminoAlTesoro mi) 
                                              else Der : (caminoAlTesoro md)

-- caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- -- Indica el camino de la rama más larga.


-- tesorosPorNivel :: Mapa -> [[Objeto]]
-- -- Devuelve los tesoros separados por nivel en el árbol.


-- todosLosCaminos :: Mapa -> [[Dir]]
-- -- Devuelve todos lo caminos en el mapa.
