data Organizador = MkO (Map Checksum (Set Persona))
                       (Map Persona (Set Checksum))
                       
-- Sea (MkO mcp mpc) un organizador.
-- * Todo checksum que sea clave en mcp y tenga como valor asociado un sp, debe pertenecer al valor asociado en mpc de cada una de las personas del sp.
-- * Toda persona que sea clave en mpc y tenga como valor asociado un sc, debe pertenecer al valor asociado en mcp de cada uno de los checkums de cp.
-- * Todo checksum asociado como valor en mpc, debe pertenecer como clave en mcp.
-- * Toda persona asociada como valor en mcp, debe pertenecer como clave en mpc.

nuevo :: Organizador -- O(1)
nuevo = MkO emptyM emptyM

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador -- O(P*(logP+logC))
agregarPrograma (MkO mc mp c') c sp = case lookupM c mc of
  Nothing -> MkO (assocM c sp mc) (agregarProgramaA c (setToList sp) mp) (newMaybe c' mc c sp)
  Just  _ -> error "El programa ya existe"

agregarProgramaA :: Checksum -> [Persona] -> Map Persona (Set Checksum) -> Map Persona (Set Checksum) -- O(P*(logP+logC))
agregarProgramaA _ []     mp = mp
agregarProgramaA c (p:ps) mp = case lookupM p mp of
  Nothing -> assocM p (addS c emptyS) (agregarProgramaA c ps mp)
  Just sc -> assocM p (addS c sc) (agregarProgramaA c ps mp)

newMaybe :: Maybe Checksum -> Map Checksum (Set Persona) -> Checksum -> Set Persona -> Maybe Checksum -- O(log C)
newMaybe m mc c sp = case m of
  Nothing -> Just c
  Just c' -> case lookupM c' mc of
    Nothing  -> error "Se supone que existe el checksum"
    Just sp' -> if sizeS sp > sizeS sp' then Just c else Just c'

todosLosProgramas :: Organizador -> [Checksum] -- O(log C)
todosLosProgramas (MkO mc _ _) = keys mc

autoresDe :: Organizador -> Checksum -> Set Persona -- O(log C)
autoresDe (MkO mc _ _) c = case lookupM c mc of
  Nothing -> error "No existe el programa"
  Just sp -> sp

programasDe :: Organizador -> Persona -> Set Checksum -- O(log P)
programasDe (MkO _ mp _) p = case lookupM p mp of
  Nothing -> error "No existe la persona"
  Just sc -> sc

programaronJuntas :: Organizador -> Persona -> Persona -> Bool -- O(log P + C log C)
programaronJuntas org p p' = if p==p' then error "Son la misma persona" else
  not (isEmptyS (intersection (programasDe org p) (programasDe org p')))

nroProgramasDePersona :: Organizador -> Persona -> Int -- O(log P)
nroProgramasDePersona org p = sizeS (programasDe org p)

elMayorPrograma :: Organizador -> Maybe Checksum -- O(1)
elMayorPrograma (MkO _ _ c) = c

programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum -- O(log P + C log C)
programasEnComun p p' org = intersection (programasDe org p) (programasDe org p')

esUnGranHacker :: Organizador -> Persona -> Bool -- O(C + log P)
esUnGranHacker org p = lenght (todosLosProgramas org) == nroProgramasDePersona org p