-- NÚMEROS ENTEROS

sucesor :: Int -> Int
-- Dado un número devuelve su sucesor
sucesor n = n + 1

sumar :: Int -> Int -> Int
-- Dados dos números devuelve su suma utilizando la operación +.
sumar n m = n + m

divisionYResto :: Int -> Int -> (Int, Int)
-- Dado dos números, devuelve un par donde la primera componente es la división del primero por el segundo, y la segunda componente es el resto de dicha división. Nota: para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int, provista por Haskell.
divisionYResto n m =  (div n m, mod n m)

maxDelPar :: (Int, Int) -> Int
-- Dado un par de números devuelve el mayor de estos.
maxDelPar (n, m) = if n > m 
                    then n 
                    else m

-- TODO ejemplos

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
-- Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe la siguiente dirección a Oeste. ¿Posee una precondición esta función? ¿Es una función total o parcial? ¿Por qué?.
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "No existe una dirreción siguiente al Oeste"

-- TODO . ¾Posee una precondición esta función? ¾Es una funcióntotal o parcial? ¾Por qué?

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