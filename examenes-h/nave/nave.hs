data Nave = N (Map SectorId Sector) 
              (Map Nombre Tripulante)
              (MaxHeap Tripulante)
              
-- Sea (N ms mt ht) una Nave.
-- * La clave de mt es el nombre del tripulante asociado como valor.
-- * La clave de ms es el id del sector asociado como valor.
-- * Todo tripulante asociado como valor en mt, es elemento de ht.
-- * Todo tripulante elemento de ht, esta asociado como valor en mt.
-- * ht no tiene elementos repetidos.
-- * Todo triplante que pertenezca a un sector asociado como valor en ms, pertenece a mt.
-- * Todo triplante que pertenezca a un sector asociado como valor en ms, pertenece a ht.

construir :: [SectorId] -> Nave -- O(S)
construir []       = N emptyM emptyM emptyH
construir (id:ids) = addSector id (construir ids)

addSector :: SectorId -> Nave -> Nave -- O(logS)
addSector id (N ms mt ht) = let newS = crearS id
                             in N (assocM id newS ms) mt ht

ingresarT :: Nombre -> Rango -> Nave -> Nave -- O(log T)
ingresarT n r (N ms mt ht) = let newT = crearT n r
                              in N ms (assocM n newT mt) (insertH newT ht)

sectoresAsignados :: Nombre -> Nave -> Set SectorId -- O(log M)
sectoresAsignados n (N _ mt _) = case lookupM n mt of
  Nothing -> error "No existe el tripulante"
  Just t -> sectoresT t

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente]) -- O(log S)
datosDeSector id (N ms _ _) = case lookupM id ms of
  Nothing -> error "No existe el sector"
  Just s -> (tripulantesS s, componentesS s)

tripulantesN :: Nave -> [Tripulante] -- O(T log T)
tripulantesN (N _ _ ht) = heapToList ht

heapToList :: MaxHeap Tripulante -> [Tripulante] -- O(T log T)
heapToList ht = if isEmptyH ht then [] else maxH ht : heapToList (deleteMaxH ht)

agregarASector :: [Componente] -> SectorId -> Nave -> Nave -- O(C + log S)
agregarASector cs id (N ms mt ht) = case lookupM id ms of
  Nothing -> error "No existe el sector"
  Just s -> let s' = agregarComponentes cs s in
    N (assocM id s' ms) mt ht

asignarASector :: Nombre -> SectorId -> Nave -> Nave -- O(log S + log T + T log T)
asignarASector n id (N ms mt ht) = case lookupM n mt of
  Nothing -> error "No existe el tripulante"
  Just t  -> let t' = asignarS id t in case lookupM id ms of
    Nothing -> error "No existe el sector"
    Just s  -> let s' = agregarT n s in
      N (assocM id s' ms) (assocM n t' mt) (insertH t' (removeH n ht))

agregarComponentes :: [Componente] -> Sector -> Sector -- O(C)
agregarComponentes [] s = s
agregarComponentes (c:cs) s = agregarC c (agregarComponentes cs s)

removeH :: Nombre -> MaxHeap Tripulante -> MaxHeap Tripulante -- O(T log T)
removeH n ht = if nombre (maxH ht) == n
               then deleteMaxH ht
               else insertH (maxH ht) (removeH n (deleteMaxH ht))

sectores :: Nave -> Set SectorId -- O(T*(S logS + log T))
sectores n = sectoresDe (tripulantesN n) n

sectoresDe :: [Tripulante] -> Nave -> Set SectorId -- O(T*(S logS + log T))
sectoresDe [] _ = emptyS
sectoresDe (t:ts) n = unionS (sectoresAsignados (nombre t) n) (sectoresDe ts n)

sinSectoresAsignados :: Nave ->[Tripulante] -- O(T log T)
sinSectoresAsignados n = tripulantesSinSector (tripulantesN n) n

tripulantesSinSector :: [Tripulante] -> Nave -> [Tripulante] --O(T log T)
tripulantesSinSector [] _ = []
tripulantesSinSector (t:ts) n = let size = sizeS (sectoresAsignados (nombre t) n)
                                    in singularSi t (size==0) ++ tripulantesSinSector ts n

barriles :: Nave -> [Barril] -- O(S * C + T * S logS + T logT)
barriles n = barrilesS (setToList (sectores n)) n

barrilesS :: [SectorId] -> Nave -> [Barril] -- O(S*(C + logS))
barrilesS []       _ = []
barrilesS (id:ids) n = barrilesC (snd (datosDeSector id n)) ++ barrilesS ids n

barrilesC :: [Componente] -> [Barril] --O(C)
barrilesC []     = []
barrilesC (c:cs) = barrillesSiEsAlmacen c ++ barrilesC cs

barrillesSiEsAlmacen :: Componente -> [Barril] -- O(1)
barrillesSiEsAlmacen (Almacen bs) = bs
barrillesSiEsAlmacen _            = []

singularSi :: a -> Bool -> [a] -- O(1)
singularSi x True  = [x]
singularSi _ False = []