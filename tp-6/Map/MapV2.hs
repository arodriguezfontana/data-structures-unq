module MapV2(Map, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ) where

data Map k v = M [(k,v)]
-- Inv. Rep.
-- Sea (M kvs) un Map y kvs una lista de tuplas de claves asociadas a valores.
-- * Cada clave de cada tupla de kvs esta asociada al respectivo valor de su tupla.
-- * Cada valor de cada tupla de kvs esta asociada a la respectiva clave de su tupla.
-- * Existen claves repetidas en las asociaciones de tuplas.

emptyM :: Map k v
-- Propósito: devuelve un map vacío.
emptyM = M []

assocM :: Eq k => k -> v -> Map k v -> Map k v
-- Propósito: agrega una asociación clave-valor al map.
assocM k v (M kvs) = M ((k,v):kvs))

lookupM :: Eq k => k -> Map k v -> Maybe v
-- Propósito: encuentra un valor dado una clave.
lookupM k (M kvs) = lookup k kvs

deleteM :: Eq k => k -> Map k v -> Map k v
-- Propósito: borra una asociación dada una clave.
deleteM k (M kvs) = M (delete k kvs)

keys :: Map k v -> [k]
-- Propósito: devuelve las claves del map.
keys (M kvs) = claves kvs

-- SUBTAREA

lookup :: Eq k => k -> [(k,v)] -> Maybe v
lookup _ [] = Nothing
lookup k ((kt,vt):kvs) = if k == kt
                            then Just vt
                            else lookup k kvs

delete :: Eq k => k -> [(k,v)] -> [(k,v)]
delete _ [] = []
delete k ((kt,vt):kvs) = if k == kt
                            then delete k kvs
                            else (kt,vt) : delete k kvs

claves :: Eq k => [(k,v)] -> [k]
claves [] = []
claves ((k,v):kvs) = k : claves kvs