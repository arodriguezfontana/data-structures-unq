import MapV1

valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = values (keys m) m

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] m = True
todasAsociadas (k:ks) m = elem k (keys m) && todasAsociadas ks m

listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap [(k,v):kvs] = assocM k v (listToMap kvs)

mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = mapToListWithKeys (keys m) m

agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq ((k,v):kvs) = case lookupM k (agruparEq kvs) of
                            Nothing -> assocM k [v] (agruparEq kvs)
                            Just vm -> assocM k (v:vm) (agruparEq kvs)

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar [] _ = emptyM
incrementar (k:ks) m = case lookupM k m of
                            Nothing -> incrementar ks m
                            Just v -> assocM k (v+1) (incrementar ks (deleteM m))

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = mergeMapsWithKeys (keys m1) m1 m2

values :: Eq k => [k] -> Map k v -> [Maybe v]
values [] _ = []
values (k:ks) m = lookupM k m : (values ks m)

mapToListWithKeys :: Eq k => [k] -> Map k v -> [(k, v)]
mapToListWithKeys [] _ = []
mapToListWithKeys (k:ks) m = (k, fromJust(lookupM k)) : (mapToListWithKeys ks m)

mergeMapsWithKeys ::  Eq k => [k] -> Map k v -> Map k v -> Map k v
mergeMapsWithKeys [] _ m2 = m2
mergeMapsWithKeys (k:ks) m1 m2 = assocM k (fromJust (lookupM k m1)) (mergeMapsWithKeys ks m1 m2)

multiAssocM :: Eq k => [(k,v)] -> Map k v -> Map k v
multiAssocM [] m = m
multiAssocM ((k,v):kvs) m = assocM k v (multiAssocM kvs m)

indexar :: [a] -> Map Int a
indexar xs = assocIndex (reverse xs)

assocIndex :: [a] -> Map Int a
assocIndex []     = emptyM
assocIndex (x:xs) = assocM (1 + length xs) x (assocIndex xs)

ocurrencias :: String -> Map Char Int
ocurrencias []     = emptyM
ocurrencias (x:xs) = assocM x (1 + apariciones x xs) (ocurrencias (remove x xs))