-- RECCURSIÓN SOBRE LISTAS

sumatoria :: [Int] -> Int
-- Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

longitud :: [a] -> Int
-- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee.
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores [] = []
sucesores (n:ns) = n + 1 : sucesores ns

conjuncion :: [Bool] -> Bool
-- Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion [] = True
conjuncion (b:bs) = b && conjuncion bs

disyuncion :: [Bool] -> Bool
-- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion [] = False
disyuncion (b:bs) = b || disyuncion bs

aplanar :: [[a]] -> [a]
-- Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss

pertenece :: Eq a => a -> [a] -> Bool
-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece e [] = False
pertenece e (x:xs) = e == x || pertenece e xs

apariciones :: Eq a => a -> [a] -> Int
-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones e [] = 0
apariciones e (x:xs) = unoSi(e == x) + apariciones e xs

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

losMenoresA :: Int -> [Int] -> [Int]
-- Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA n [] = []
losMenoresA n (m:ms) = singularSi m (m<n) ++ losMenoresA n ms

singularSi :: a -> Bool -> [a]
singularSi x True = [x]
singularSi _ _ = []

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
-- Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (xs:xss) = singularSi xs (longitud xs > n) ++ lasDeLongitudMayorA n xss

agregarAlFinal :: [a] -> a -> [a]
-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
agregarAlFinal [] y = [y]
agregarAlFinal (x:xs) y = x : agregarAlFinal xs y

-- CONSULTA
agregar :: [a] -> [a] -> [a]
-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los elementos de la segunda a continuación. Definida en Haskell como (++).
agregar [] ys = ys
agregar (x:xs) ys = x : agregar xs ys

reversa :: [a] -> [a]
-- Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse.
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x

zipMaximos :: [Int] -> [Int] -> [Int]
-- Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que las listas no necesariamente tienen la misma longitud.
zipMaximos xs [] = xs
zipMaximos [] ys = ys
zipMaximos (x:xs) (y:ys) = maximoEntre x y : zipMaximos xs ys

maximoEntre :: Int -> Int -> Int
maximoEntre n m = if n > m
                   then n 
                   else m

elMinimo :: Ord a => [a] -> a
-- Dada una lista devuelve el mínimo.
elMinimo [] = error "La lista está vacía."
elMinimo [x] = x
elMinimo (x:xs) = min x (elMinimo xs)

-- RECURSIÓN SOBRE NÚMEROS

factorial :: Int -> Int
-- Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
-- Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
-- Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir 0 _ = []
repetir n x = x : repetir (n-1) x

losPrimeros :: Int -> [a] -> [a]
-- Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs. Si la lista es vacía, devuelve una lista vacía.
losPrimeros 0 _ = [] 
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a]
-- Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros 0 xs = xs
sinLosPrimeros _ [] = []
sinLosPrimeros n (_:xs) = sinLosPrimeros (n-1) xs

-- REGISTROS

type Nombre = String
type Edad = Int
data Persona = P Nombre Edad deriving Show

mayoresA :: Int -> [Persona] -> [Persona]
-- Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA _ [] = []
mayoresA n (p:ps) = if edad p > n
                     then p : mayoresA n ps
                     else mayoresA n ps

edad :: Persona -> Int
-- Devuelve la edad de una persona.
edad (P _ e) = e

-- CONSULTA
-- promedioEdad :: [Persona] -> Int
-- Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.

elMasViejo :: [Persona] -> Persona
-- Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la lista al menos posee una persona.
elMasViejo [] = error "La lista está vacía."
elMasViejo [p] = p 
elMasViejo (p:ps) = laQueEsMayor p (elMasViejo ps)

laQueEsMayor :: Persona -> Persona -> Persona
-- Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
                      then p1
                      else p2

esMayorQueLaOtra :: Persona -> Persona -> Bool
-- Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra p1 p2 = edad(p1) > edad(p2)

type Energia = Int
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = Pk TipoDePokemon Energia deriving Show
data Entrenador = E Nombre [Pokemon] deriving Show

p1 = Pk Agua 33
p2 = Pk Planta 53
p3 = Pk Fuego 63

e = E "abi" [p1,p2,p3,p3,p3]

cantPokemon :: Entrenador -> Int
-- Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon (E _ ps) = longitud ps

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe t (E _ ps) = cantPokemonDeL t ps

cantPokemonDeL :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDeL _ [] = 0
cantPokemonDeL t (p:ps) = unoSi (sonMismoTipo t (tipo p)) + cantPokemonDeL t ps

sonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonMismoTipo Agua Agua = True
sonMismoTipo Fuego Fuego = True
sonMismoTipo Planta Planta = True
sonMismoTipo _ _ = False

tipo :: Pokemon -> TipoDePokemon
tipo (Pk t _) = t

cuantosDeTipoDeLeGananATodosLosDe :: TipoDePokemon -> Entrenador -> Entrenador -> Int
Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo pertenecientes al primer entrenador, que le ganarían a todos los Pokemon del segundo entrenador.

esMaestroPokemon :: Entrenador -> Bool
-- Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
