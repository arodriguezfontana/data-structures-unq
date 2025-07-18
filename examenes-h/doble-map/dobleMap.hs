data DoubleMap k v = DM (Map k (Map k v)) k1 k2
                        (Map k (Map k v)) k2 k1

-- Sea (DM m1 m2) un doble map.
-- * Toda clave de m1, es clave del map asociado como valor en m2.
-- * Toda clave del map asociado como valor en m2, es clave m1.
-- * Toda clave de m2, es clave del map asociado como valor en m1.
-- * Toda clave del map asociado como valor en m1, es clave m2.

emptyDM :: DoubleMap k v -- O(1)
emptyDM = DM emptyM emptyM

assocDM :: Eq k => k -> k -> v -> DoubleMap k v -> DoubleMap k v -- O(log K1 + log K2)
assocDM k1 k2 v (DM m1 m2) = DM (mapModificado k1 k2 v m1) (mapModificado k2 k1 v m2) 

lookupDM :: Eq k => Either k k -> DoubleMap k v -> [(k,v)] -- O(max (log K1 + K2 log K2) (log K2 + K1 log K1))
lookupDM (Left k1) (DM m1 _) = lookup k1 m1
lookupDM (Right k2) (DM _ m2) = lookup k2 m2

keyesDM :: DoubleMap k v -> ([k], [k]) -- O(K1 + K2)
keyesDM (DM m1 m2) = (keys m1, keys m2)

docentesCalificados :: Calificaciones -> [Nombre] -- O(K1 + K2)
docentesCalificados c = fst(keyesDM c)

topNMasAfines :: Calificaciones -> Nombre -> Int -> [(Int, Nombre)] -- O((N + K12) log K12 + log K2 + K1 log K1)
topNMasAfines c e n = case lookupDM (Right e) c of
                        Nothing -> []
                        Just kvs -> takeNDeH n (listToHeapPorAfinidad kvs)

listToHeapPorAfinidad :: [(Nombre, (Int, Int))] -> Heap (Int, Nombre)
listToHeapPorAfinidad [] = emptyH
listToHeapPorAfinidad ((n,ta):ntas) = insertH (afinidad ta, n) (listToHeapPorAfinidad ntas)

takeNDeH :: Int -> Heap (Int, Nombre) -> [(Int, Nombre)]
takeNDeH 0 h = []
takeNDeH n h = findMaxH h : (takeNDeH (n-1) (deleteMaxH h))

afinidad :: (Int, Int) -> Int -- Calcula la afinidad entre dos calificaciopnes reciprocas. 

mapModificado :: Eq k => k -> k -> v -> Map k (Map k v) -> Map k (Map k v)
mapModificado k1 k2 v m = case lookupM k1 m of
                            Nothing -> assocM k1 (assocM k2 v emptyM) m 
                            Just m2 -> assocM k1 (assocM k2 v m2) m

lookup :: Eq k => k -> Map k (Map k v) -> [(k,v)]
lookup k m = case lookupM k m of
                Nothing -> []
                Just m2 -> kvs (keys m2) m2

kvs :: [k] -> Map k v -> [(k,v)]
kvs [] m = []
kvs (k:ks) m = case lookupM k m of
                Nothing -> []
                Just v -> (k,v) : kvs ks m