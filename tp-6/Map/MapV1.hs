module MapV1(Map, emptyM, assocM, lookupM, deleteM, keys) where

data Map k v = M [(k,v)]
-- Inv. Rep.
-- Sea (M kvs) un Map y kvs una lista de tuplas de claves asociadas a valores.
-- * Cada clave de cada tupla de kvs esta asociada al respectivo valor de su tupla.
-- * Cada valor de cada tupla de kvs esta asociada a la respectiva clave de su tupla.
-- * No existe una clave repetida en la asociacion de otra tupla en kvs.

emptyM :: Map k v
emptyM = M []

assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M kvs) = M (assoc k v kvs)

lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M kvs) = lookup k kvs

deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M kvs) = M (delete k kvs)

keys :: Map k v -> [k]
keys (M kvs) = claves kvs

assoc :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
assoc k v [] = [(k,v)]
assoc k v ((kt,vt):kvs) = if k == kt
                            then (kt,v) : kvs
                            else (kt,vt) : assoc k v kvs

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