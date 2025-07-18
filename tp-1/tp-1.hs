sucesor :: Int -> Int
sucesor n = n + 1

sumar :: Int -> Int -> Int
sumar n m = n + m

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if n > m 
                    then n 
                    else m
                    
data Dir = Norte | Este | Sur | Oeste deriving Show

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "No existe una dirección siguiente al Oeste."

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDia, ultimoDia)

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = diaANumero(d1) > diaANumero(d2)

diaANumero :: DiaDeSemana -> Int
diaANumero Lunes = 1
diaANumero Martes = 2
diaANumero Miercoles = 3
diaANumero Jueves = 4
diaANumero Viernes = 5
diaANumero Sabado = 6
diaANumero Domingo = 7

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

negar :: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True a = a
implica False _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True a = a
yTambien False _ = False

oBien :: Bool -> Bool -> Bool
oBien False a = a
oBien True _ = True

type Nombre = String
type Edad = Int
data Persona = P Nombre Edad deriving Show

nombre :: Persona -> String
nombre (P n _) = n

edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre n2 (P _ e) = P n2 e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad(p1) > edad(p2)

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
                      then p1
                      else p2

type Energia = Int
data Pokemon = Pk TipoDePokemon Energia deriving Show
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Entrenador = E Nombre Pokemon Pokemon deriving Show

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esSuperior (tipo p1) (tipo p2)

tipo :: Pokemon -> TipoDePokemon
tipo (Pk t _) = t

esSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esSuperior Agua Fuego = True
esSuperior Fuego Planta = True
esSuperior Planta Agua = True
esSuperior _ _ = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E _ p1 p2) = unoSi(sonMismoTipo t (tipo p1)) + unoSi(sonMismoTipo t (tipo p2))

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

sonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonMismoTipo Agua Agua = True
sonMismoTipo Fuego Fuego = True
sonMismoTipo Planta Planta = True
sonMismoTipo _ _ = False

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = pokemonsDe e1 ++ pokemonsDe e2

pokemonsDe :: Entrenador -> [Pokemon]
pokemonsDe (E _ p1 p2) = [p1, p2]

loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete _ = 7

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
elPrimero [] = error "La lista está vacía."
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
sinElPrimero [] = error "La lista está vacía."
sinElPrimero (_:xs) = xs

splitHead :: [a] -> (a, [a])
splitHead [] = error "La lista está vacía."
splitHead (x:xs) = (x, xs)