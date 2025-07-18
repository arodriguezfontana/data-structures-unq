data Nave = MkN (Map Sector (Set Tripulante))
                (Heap Tripulante)
                (Sector, Int)

-- Sea (MkN mst ht (s,i)) una nave.
-- * s es el sector con mayor cantidad tripulantes e i, la cantidad de tripulantes de s.
-- * ht no tiene tripulantes reprtidos. 
-- * Para cada sector sc clave en mst, la cantidad de tripulantes de sc es menor o igual a la cantidad de tripulantes de s.
-- * Para todo tripulante t que pertenezca a un conjunto st asociado como valor en mst para un sector, t no puede pertenecer a otro conjunto que este asociado como valor en otro sector de mst.
-- * Para todo tripulante t que pertenezca a un conjunto st asociado como valor en mst, t debe pertenecer a ht.

naveVacia :: [Sector] -> Nave -- O(S log S)
naveVacia ss = MkN (assocSectores ss) emptyH (head ss, 0)

assocSectores :: [Sector] -> Map Sector (Set Tripulante) -- O(log S)
assocSectores [] = emptyM
assocSectores (s:ss) = assocM s emptyS (assocSectores ss)

tripulantesDe :: Sector -> Nave -> Set Tripulante -- O(log S)
tripulantesDe s (MkN ms _ _) = case lookupM s ms of
  Nothing -> emptyS
  Just st -> st

sectores :: Nave -> [Sector] -- O(S)
sectores (MkN ms _ _) = keys ms

conMayorRango :: Nave -> Tripulante -- O(1)
conMayorRango (MkN _ ht _) = findMin ht

conMasTripulantes :: Nave -> Sector -- O(1)
conMasTripulantes (MkN _ _ (s,_)) = s

conRango :: Rango -> Nave -> Set Tripulante -- O(P log P)
conRango r (MkN _ ht _) = conRangoH r ht

conRangoH :: Rango -> Heap Tripulante -> Set Tripulante -- O(P log P)
conRangoH r ht = if isEmptyH ht then [] else
                 if r == rango (findMin ht)
                   then addS (findMin ht) (conRangoH r (deleteMin ht))
                   else conRangoH r (deleteMin ht)

sectorDe :: Tripulante -> Nave -> Sector -- O(S log S log P)
sectorDe t (MkN ms _ _) = sectorT t (keys ms) ms

sectorT :: Tripulante -> [Sector] -> Map Sector (Set Tripulante) -> Sector -- O(log S log P)
sectorT t []     ms = error "No existe un sector asignado a ese tripulante" 
sectorT t (s:ss) ms = case lookupM s ms of
  Nothing -> error "No existe el sector"
  Just st -> if belongs t st then s else sectorT t ss ms