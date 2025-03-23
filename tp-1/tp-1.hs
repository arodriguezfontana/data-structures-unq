-- NÚMEROS ENTEROS

sucesor :: Int -> Int
-- Dado un número devuelve su sucesor
sucesor n = n + 1

sumar :: Int -> Int -> Int
-- Dados dos números devuelve su suma utilizando la operación +.
sumar n m = n + m

divisionYResto :: Int -> Int -> (Int, Int)
-- Dado dos números, devuelve un par donde la primera componente es la división del primero por el segundo, y la segunda componente es el resto de dicha división. Nota: para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int, provista por Haskell.
divisionYResto n m = (div n m, mod n m)

maxDelPar :: (Int, Int) -> Int
-- Dado un par de números devuelve el mayor de estos.
maxDelPar (n, m) = if n > m 
                    then n 
                    else m
                    
-- TIPOS ENUMERATIVOS

data Dir = Norte | Este | Sur | Oeste deriving Show

opuesto :: Dir -> Dir
-- Dada una dirección devuelve su opuesta.
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
-- Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

siguiente :: Dir -> Dir
-- Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe la siguiente dirección a Oeste.
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "No existe una dirección siguiente al Oeste."

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
-- Devuelve un par donde la primera componente es el primer día de la semana, y la segunda componente es el último día de la semana. Considerar definir subtareas útiles que puedan servir después.
primeroYUltimoDia = (primerDia, ultimoDia)

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

empiezaConM :: DiaDeSemana -> Bool
-- Dado un día de la semana indica si comienza con la letra M.
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
-- Dado dos días de semana, indica si el primero viene después que el segundo. Analizar la calidad de la solución respecto de la cantidad de casos analizados (entre los casos analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).
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
-- Dado un día de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

negar :: Bool -> Bool
-- Dado un booleano, si es True devuelve False, y si es False devuelve True. En Haskell ya está definida como not.
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
-- Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino devuelve True. Esta función NO debe realizar doble pattern matching. Nota: no viene implementada en Haskell.
implica True a = a
implica False _ = True

yTambien :: Bool -> Bool -> Bool
-- Dados dos booleanos si ambos son True devuelve True, sino devuelve False. Esta función NO debe realizar doble pattern matching. En Haskell ya está definida como &&. 
yTambien True a = a
yTambien False _ = False

oBien :: Bool -> Bool -> Bool
-- Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False. Esta función NO debe realizar doble pattern matching. En Haskell ya está definida como ||.
oBien False a = a
oBien True _ = True

-- REGISTROS

type Nombre = String
type Edad = Int
data Persona = P Nombre Edad deriving Show

nombre :: Persona -> String
-- Devuelve el nombre de una persona.
nombre (P n _) = n

edad :: Persona -> Int
-- Devuelve la edad de una persona.
edad (P _ e) = e

crecer :: Persona -> Persona
-- Aumenta en uno la edad de la persona.
crecer (P n e) = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
-- Dados un nombre y una persona, devuelve una persona con la edad de la persona y el nuevo nombre.
cambioDeNombre n2 (P _ e) = P n2 e

esMayorQueLaOtra :: Persona -> Persona -> Bool
-- Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra p1 p2 = edad(p1) > edad(p2)

laQueEsMayor :: Persona -> Persona -> Persona
-- Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
                      then p1
                      else p2

type Energia = Int
data Pokemon = Pk TipoDePokemon Energia deriving Show
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Entrenador = E Nombre Pokemon Pokemon deriving Show

superaA :: Pokemon -> Pokemon -> Bool
-- Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
superaA p1 p2 = esSuperior (tipo p1) (tipo p2)

tipo :: Pokemon -> TipoDePokemon
tipo (Pk t _) = t

esSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esSuperior Agua Fuego = True
esSuperior Fuego Planta = True
esSuperior Planta Agua = True
esSuperior _ _ = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
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
-- Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon (e1, e2) = pokemonsDe e1 ++ pokemonsDe e2

pokemonsDe :: Entrenador -> [Pokemon]
pokemonsDe (E _ p1 p2) = [p1, p2]

-- FUNCIONES POLIMÓRFICAS

loMismo :: a -> a
-- Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo x = x

siempreSiete :: a -> Int
-- Dado un elemento de algún tipo devuelve el número 7.
siempreSiete _ = 7

swap :: (a, b) -> (b, a)
-- Dadas una tupla, invierte sus componentes.
swap (x, y) = (y, x)

-- PATTERN MATCHING SOBRE LISTAS

estaVacia :: [a] -> Bool
-- Dada una lista de elementos, si es vacía devuelve True, sino devuelve False. Definida en Haskell como null.
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
-- Dada una lista devuelve su primer elemento. Definida en Haskell como head.
elPrimero [] = error "La lista está vacía."
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
-- Dada una lista devuelve esa lista menos el primer elemento. Definida en Haskell como tail.
sinElPrimero [] = error "La lista está vacía."
sinElPrimero (_:xs) = xs

splitHead :: [a] -> (a, [a])
-- Dada una lista devuelve un par, donde la primera componente es el primer elemento de la lista, y la segunda componente es esa lista pero sin el primero.
splitHead [] = error "La lista está vacía."
splitHead (x:xs) = (x, xs)