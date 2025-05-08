import MapV1
-- import MapV2
-- import MapV3

valuesM :: Eq k => Map k v -> [Maybe v]
-- Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = values (keys m) m

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
-- Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas [] m = True
todasAsociadas (k:ks) m = elem k (keys m) && todasAsociadas ks m

listToMap :: Eq k => [(k, v)] -> Map k v
-- Propósito: convierte una lista de pares clave valor en un map.
listToMap [] = emptyM
listToMap [(k,v):kvs] = assocM k v (listToMap kvs)

mapToList :: Eq k => Map k v -> [(k, v)]
-- Propósito: convierte un map en una lista de pares clave valor.
mapToList m = mapToListWithKeys (keys m) m

agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq [] = emptyM
agruparEq ((k,v):kvs) = case lookupM k (agruparEq kvs) of
                            Nothing -> assocM k [v] (agruparEq kvs)
                            Just vm -> assocM k (v:vm) (agruparEq kvs)

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
incrementar [] _ = emptyM
incrementar (k:ks) m = case lookupM k m of
                            Nothing -> incrementar ks m
                            Just v -> assocM k (v+1) (incrementar ks (deleteM m))

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps m1 m2 = mergeMapsWithKeys (keys m1) m1 m2

-- SUBTAREAS

values :: Eq k => [k] -> Map k v -> [Maybe v]
values [] _ = []
values (k:ks) m = lookupM k m : (values ks m)

mapToListWithKeys :: Eq k => [k] -> Map k v -> [(k, v)]
-- Precondición La lista de claves deben claves que pertenecen al map dado.
mapToListWithKeys [] _ = []
mapToListWithKeys (k:ks) m = (k, fromJust(lookupM k)) : (mapToListWithKeys ks m)

mergeMapsWithKeys ::  Eq k => [k] -> Map k v -> Map k v -> Map k v
-- Precondición La lista de claves deben claves que pertenecen al primer map dado.
mergeMapsWithKeys [] _ m2 = m2
mergeMapsWithKeys (k:ks) m1 m2 = assocM k (fromJust (lookupM k m1)) (mergeMapsWithKeys ks m1 m2)

-- indexar :: [a] -> Map Int a
-- -- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con su posición en la lista.

-- ocurrencias :: String -> Map Char Int
-- -- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen en el string, y los valores la cantidad de veces que aparecen en el mismo.