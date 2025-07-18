data EscuelaDeMagia = EDM (Set Hechizo)
                          (Map Nombre Mago)
                          (PriorityQueue Mago)
                          
-- Sea (EDM sh mm pm) una escuela de magia.
-- * La clave de mm es el respectivo nombre del mago asociado como valor.
-- * Todo mago asociado como valor en mm, debe ser elemento de pm.
-- * Todo mago elemento de pm, debe estar asociado como valor en mm.
-- * Todos los hechizos de cada mago deben pertenecer a sh.
-- * La pm no tiene dos magos con el mismo nombre, es decir, no acepta repetidos.

fundarEscuela   :: EscuelaDeMagia -- O(1)
fundarEscuela = EDM emptyS emptyM emptyPQ

estaVacia :: EscuelaDeMagia -> Bool -- O(1)
estaVacia (EDM _ _ p) = isEmptyPQ p

magos :: EscuelaDeMagia -> [Nombre] -- O(M)
magos (EDM _ mm _) = keys mm

registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia -- O(log M)
registrar n (EDM s mm p) = 
    case (lookupM n mm) of
      Nothing -> let m = crearM n
                  in EDM s (assocM n m mm) (insertPQ m p)
      Just _  -> EDM s mm p

hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo -- O(log M)   
hechizosDe n (EDM s mm p) = buscarHechizos n mm

leFaltaAprender :: Nombre -> EscuelaDeMagia -> Int -- O(log M)
leFaltaAprender n e@(EDM s mm _) = sizeS s - sizeS (hechizosDe n e)

buscarHechizos :: Nombre -> Map Nombre Mago -> Set Hechizo -- O(log M)
buscarHechizos n mm = case (lookupM n mm) of
                        Nothing -> error "No estudia acá"
                        Just m  -> hechizos m

egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia) -- O(log M)
egresarUno (EDM s mm p) = 
    let m = findMinPQ p
     in (m, EDM s (deleteM (nombre m) mm) (deleteMinPQ p)) 

enseniar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia -- O(M log M + log H)
enseniar h n (EDM s mm p) = 
    case (lookupM n mm) of
      Nothing -> error "No estudia acá"
      Just m  -> let newM = aprender h m
                  in EDM (addS h s) (assocM n newM mm)
                         (modificarPQ newM p)

modificarPQ :: Mago -> PriorityQueue Mago -> PriorityQueue Mago -- O(M log M)
-- Precondición: tiene que haber un mago con el mismo nombre que el mago dado en la cola                    
modificarPQ m p = let mmp = findMinPQ p
                   in if (m == mmp)
                       then insertPQ m (deleteMinPQ p)
                       else insertPQ mmp (modificarPQ m (deleteMinPQ p))