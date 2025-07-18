data RAList a = MkR Int 
                    (Map Int a) 
                    (Heap a)
                    
-- Sea (MkR n m h) una RAList.
-- * Todos los valores asociados en m, deben ser elementos de h.
-- * Los elementos de h, deben pertenecer como valor asociado en m.
-- * n representa la proxima posición a ocupar en la lista.
-- * n es la cantidad de elementos de la estructura, por ende de elementos en m y h.
-- * n >= 0.

-- K = H = N representan la cantidad de elementos de la RAList.

emptyRAL :: RAList a -- O(1)
emptyRAL = MkR 0 emptyM emptyH

isEmptyRAL :: RAList a -> Bool -- O(1)
isEmptyRAL (MkR n _ _) = n == 0

lengthRAL :: RAList a -> Int -- O(1)
lengthRAL (MkR n _ _) = n - 1

get :: Int -> RAList a -> a -- O(log N)
-- Precondición: el índice debe existir.
get i (MkR _ m _) = fromJust(lookupM i m)

minRAL :: Ord a => RAList a -> a -- O(1)
-- Precondición: la lista no está vacía.
minRAL (MkR _ _ h) = findMin h

add :: Ord a => a -> RAList a -> RAList a --O(log N)
add e (MkR n m h) = MkR (n+1) (assocM n e m) (insertH e)

elems :: Ord a => RAList a -> [a] -- O(N log N)
elems (MkR n m h) = heapToList h

remove :: Ord a => RAList a -> RAList a -- O(N log N)
-- Precondición: la lista no está vacía.
remove (MkR n m h) = let ultimo = fromJust(lookupM (n-1) m) 
                        in MkR (n-1) (deleteM (n-1) m) (deleteInH ultimo h)

set :: Ord a => Int -> a -> RAList a -> RAList a -- O(N log N)
-- Precondición: el índice debe existir.
set i e (MkR n m h) = let elemento = fromJust(lookupM i m) 
                            in MkR n (assocM i e m) (insertH e (deleteInH elemento h))

addAt :: Ord a => Int -> a -> RAList a -> RAList a -- O(N log N)
-- Precondición: el índice debe estar entre 0 y la longitud de la lista.
addAt i e (MkR n m h) = Mkr (n+1) (agregarYMover i n e m) (insertH e h)

heapToList :: Heap a -> [a] -- O(H log H)
heapToList h = findMin h : heapToList (deleteMin h)

deleteInH :: Ord a => a -> Heap a -> Heap a -- O(H log H)
deleteInH e h = if findMin h == e 
                    then deleteMin h
                    else insertH (findMin h) (deleteInH e (deleteMin h))

agregarYMover :: Ord a => Int -> Int -> a -> Map Int a -> Map Int a --O(K log k)
agregarYMover i n e m = assocM i e (mapConIndicesAumentadosDesdeIndice i n e m)

mapConIndicesAumentadosDesdeIndice :: Ord a => Int -> Int -> a -> Map Int a -> Map Int a -- O(K log k)
mapConIndicesAumentadosDesdeIndice i n m = case lookupM i m of
                                                Nothing -> m
                                                Just v -> assocM (i+1) v (mapConIndicesAumentadosDesdeIndice (i+1) n m)