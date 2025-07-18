module MapV3(Map, emptyM, assocM, lookupM, deleteM, keys) where

data Map k v = M [k] [v]
-- Inv. Rep.
-- Sea (M ks vs) un Map, ks una lista de claves y vs una lista de valores.
-- * ks y vs tienen la misma longitud.
-- * Sea i un numero natural, cada clave de ks ubicada en la posición i, esta asociada al valor en la misma posición en vs.
-- * Sea i un numero natural, Cada valor de kvs ubicado en la posición i, esta asociada a la clave en la misma posición en ks.

emptyM :: Map k v 
emptyM = M [] []

assocM :: Eq k => k -> v -> Map k v -> Map k v 
assocM k v (M ks vs) = M (k:ks) (v:vs)

lookupM :: Eq k => k -> Map k v -> Maybe v 
lookupM k (M ks vs) = valorEnPosicionDeClave k ks vs

deleteM :: Eq k => k -> Map k v -> Map k v 
deleteM k (M ks vs) = M (borrarClave k ks) (borrarValor k ks vs)

keys :: Map k v -> [k] 
keys (M ks _) = ks

valorEnPosicionDeClave :: Eq k => k -> [k] -> [v] -> Maybe v 
valorEnPosicionDeClave _ ks [] = Nothing
valorEnPosicionDeClave _ [] vs = Nothing 
valorEnPosicionDeClave k (k2:ks) (v:vs) = if k == k2 
                                            then Just v 
                                            else valorEnPosicionDeClave k ks vs

borrarClave :: Eq k => k -> [k] -> [k]
borrarClave _ [] = []
borrarClave k (k1:k1s) = if k == k1
                            then borrarClave k k1s
                            else k1 : borrarClave k k1s

borrarValor :: Eq k => k -> [k] -> [v] -> [v]
borrarValor _ ks [] = []
borrarValor _ [] vs = [] 
borrarValor k (k2:ks) (v:vs) = if k == k2 
                                then borrarValor k ks vs
                                else v : borrarValor k ks vs  