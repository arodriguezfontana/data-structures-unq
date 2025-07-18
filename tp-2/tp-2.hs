sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) = n + 1 : sucesores ns

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (b:bs) = b && conjuncion bs

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (b:bs) = b || disyuncion bs

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = e == x || pertenece e xs

apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = unoSi(e == x) + apariciones e xs

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (m:ms) = singularSi m (m<n) ++ losMenoresA n ms

singularSi :: a -> Bool -> [a]
singularSi x True = [x]
singularSi _ _ = []

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (xs:xss) = singularSi xs (longitud xs > n) ++ lasDeLongitudMayorA n xss

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] y = [y]
agregarAlFinal (x:xs) y = x : agregarAlFinal xs y

agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar (x:xs) ys = x : agregar xs ys

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos xs [] = xs
zipMaximos [] ys = ys
zipMaximos (x:xs) (y:ys) = maximoEntre x y : zipMaximos xs ys

maximoEntre :: Int -> Int -> Int
maximoEntre n m = if n > m
                   then n 
                   else m

elMinimo :: Ord a => [a] -> a
elMinimo [] = error "La lista está vacía."
elMinimo [x] = x
elMinimo (x:xs) = min x (elMinimo xs)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n < 1 
                     then []
                     else n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n x = x : repetir (n-1) x

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = [] 
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs = xs
sinLosPrimeros _ [] = []
sinLosPrimeros n (_:xs) = sinLosPrimeros (n-1) xs

type Nombre = String
type Edad = Int
data Persona = P Nombre Edad deriving Show

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (p:ps) = if edad p > n
                     then p : mayoresA n ps
                     else mayoresA n ps

edad :: Persona -> Int
edad (P _ e) = e

promedioEdad :: [Persona] -> Int
promedioEdad ps = div (sumaDeEdades ps) (longitud ps)

sumaDeEdades :: [Persona] -> Int
sumaDeEdades [] = 0
sumaDeEdades (p:ps) = edad p + sumaDeEdades ps

elMasViejo :: [Persona] -> Persona
elMasViejo [] = error "La lista está vacía."
elMasViejo [p] = p 
elMasViejo (p:ps) = laQueEsMayor p (elMasViejo ps)

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
                      then p1
                      else p2

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad(p1) > edad(p2)

type Energia = Int
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = Pk TipoDePokemon Energia deriving Show
data Entrenador = E Nombre [Pokemon] deriving Show

cantPokemon :: Entrenador -> Int
cantPokemon (E _ ps) = longitud ps

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
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

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = cantQueLeGananATodosDe t (pokemonsDe e1) (pokemonsDe e2)

pokemonsDe :: Entrenador -> [Pokemon]
pokemonsDe (E _ ps) = ps

cantQueLeGananATodosDe :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cantQueLeGananATodosDe _ [] _ = 0
cantQueLeGananATodosDe t (p:ps) ps2 = unoSi(sonMismoTipo t (tipo p) && leGanaATodos p ps2) + cantQueLeGananATodosDe t ps ps2

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos p [] = True
leGanaATodos p (p2:p2s) = esSuperior (tipo p) (tipo p2) && leGanaATodos p p2s

esSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esSuperior Agua Fuego = True
esSuperior Fuego Planta = True
esSuperior Planta Agua = True
esSuperior _ _ = False

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (E _ ps) = tienePokemonTipo Agua ps && tienePokemonTipo Fuego ps && tienePokemonTipo Planta ps

tienePokemonTipo :: TipoDePokemon -> [Pokemon] -> Bool
tienePokemonTipo _ [] = False
tienePokemonTipo t (p:ps) = sonMismoTipo t (tipo p) || tienePokemonTipo t ps

data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = Py Nombre deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = Em [Rol] deriving Show

proyectos :: Empresa -> [Proyecto]
proyectos (Em rs) = proyectosR rs

proyectosR :: [Rol] -> [Proyecto]
proyectosR [] = []
proyectosR (r:rs) = agregarSiNoSeRepite (proyectoDe r) (proyectosR rs)

agregarSiNoSeRepite :: Proyecto -> [Proyecto] -> [Proyecto]
agregarSiNoSeRepite x [] = [x]
agregarSiNoSeRepite x (y:ys) = if sonMismoProyecto x y
                                then y : ys
                                else y : agregarSiNoSeRepite x ys

sonMismoProyecto :: Proyecto -> Proyecto -> Bool
sonMismoProyecto p p2 = nombrePy p == nombrePy p2

nombrePy :: Proyecto -> Nombre
nombrePy (Py n) = n

proyectoDe :: Rol -> Proyecto
proyectoDe (Developer _ p) = p
proyectoDe (Management _ p) = p

losDevSenior :: Empresa -> [Proyecto] -> Int 
losDevSenior (Em rs) ps = cantSeniorQuePertenecenA rs ps

cantSeniorQuePertenecenA :: [Rol] -> [Proyecto] -> Int
cantSeniorQuePertenecenA [] _ = 0
cantSeniorQuePertenecenA (r:rs) ps = unoSi (esDevSenior r && perteneceAAlguno r ps) + cantSeniorQuePertenecenA rs ps

esDevSenior :: Rol -> Bool
esDevSenior (Developer s _) = esSenior s
esDevSenior _ = False

esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _ = False

perteneceAAlguno :: Rol -> [Proyecto] -> Bool
perteneceAAlguno _ [] = False
perteneceAAlguno r (p:ps) = sonMismoProyecto (proyectoDe r) p || perteneceAAlguno r ps

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (Em rs) = cantQueTrabajanEnR ps rs

cantQueTrabajanEnR :: [Proyecto] -> [Rol] -> Int
cantQueTrabajanEnR ps [] = 0
cantQueTrabajanEnR ps (r:rs) = unoSi (perteneceAAlguno r ps) + cantQueTrabajanEnR ps rs

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (Em rs) = asignadosPorProyectoR rs

asignadosPorProyectoR :: [Rol] -> [(Proyecto, Int)]
asignadosPorProyectoR [] = []
asignadosPorProyectoR (r:rs) = agregarProyectoA (proyectoDe r) (asignadosPorProyectoR rs)

agregarProyectoA :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
agregarProyectoA p1 [] = [(p1, 1)]
agregarProyectoA p1 ((p2,i):p2is) = if sonMismoProyecto p1 p2
                                    then (p2,i+1) : p2is
                                    else (p2,i) : agregarProyectoA p1 p2is