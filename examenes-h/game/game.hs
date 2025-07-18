data EmGame = AG (Map Personaje [Esmeralda])
                 (Map Esmeralda (Maybe Personaje))
                 (MaxHeap Personaje)

-- Sea (EmGame mp me hp) un game:
-- * Para todo personaje P que sea clave en mp, P debe ser elemento de hp.
-- * Para todo personaje P elemento de hp, P debe aparecer como clave en mp.
-- * Para todo personaje P que sea clave en mp y tenga como valor asociado una lista no vacia de esmeraldas, por cada esmeralda E de la lista, E aparece como clave en me con P perteneciente a su valor asociado como "Just P".
-- * Para toda esmeralda E que sea clave en me y no pertenezca a ninguna lista de esmeraldas asociada como valor para algun personaje P en mp, el valor asociade de e en me es "Nothing".
-- * Para toda esmeralda E que pertenezca a alguna lista de esmeraldas asociada como valor para algun personaje P en mp, E debe aparecer como clave en me con el respectivo P como valor asociado como "Just P".
-- * hp no acepta personajes repetidos.

iniciarJuego :: Set Personaje → Set Esmeralda → EmGame -- O(P log P + E log E)
iniciarJuego ps es = if isEmptyS ps || isEmptyS es
  then error "Faltan elemmentos iniciales"
  else AG (assocP  (set2list ps))
          (assocE  (set2list es))
          (insertP (set2list ps))

assocP :: [Personaje] -> Map Personaje [Esmeralda] -- O(P log P)
assocP []     = emptyM
assocP (p:ps) = assocM p [] (assocP ps)

assocE :: [Esmeralda] -> Map Esmeralda (Maybe Personaje) -- O(E log E)
assocE []     = emptyM
assocE (e:es) = assocM e Nothing (assocE es)

insertP :: [Personaje] -> MaxHeap Personaje -- O(P log P)
insertP []     = newH
insertP (p:ps) = insertH p (insertP ps)

elMasPoderoso :: EmGame → Personaje -- O(1)
elMasPoderoso (AG _ _ hp) = findMaxH hp

esmeraldasDe :: EmGame → Personaje → [Esmeralda] -- O(log P)
esmeraldasDe (AG mpe _ _) p = case lookupM p mpe of
  Nothing -> error "No existe el personaje"
  Just es -> es

obtenerEsmeralda :: EmGame → Personaje → Esmeralda → EmGame -- O(log P + log E)
obtenerEsmeralda (AG mpe mep hp) p e = case lookupM e mep of
  Nothing -> error "No existe la esmeralda"
  Just mp -> case mp of
    Just _  -> error "Ya está asignada"
    Nothing -> case lookupM p mpe of
      Nothing -> error "El personaje no existe"
      Just es -> let mpe' = assocM p (e:es) mpe
                     mep' = assocM e (Just p) mep
                  in AG mpe' mep' hp

ganarEsmeralda :: EmGame → Personaje → Personaje → Esmeralda -> EmGame -- O(log P + E)
ganarEsmeralda (AG mpe mep hp) p p' e = case lookupM e mpe of
  Nothing -> error "No existe la esmeralda"
  Just mp -> case mp of
    Nothing  -> error "No pertenece a ninguno de los dos"
    Just p'' -> if p /= p'' && p' /= p''
      then error "No pertenece a ninguno de los dos"
      else let (g,l) = ganadorYPerdedor p p'
               mpe'  = darEsmeraldaAlGanador g l e mpe
               mep'  = assocM e (Just g) mep
            in AG mpe' mep' hp

darEsmeraldaAlGanador :: Personaje -> Personaje -> Esmeralda -> Map Personaje [Esmeralda] -> Map Personaje [Esmeralda] -- O(log P + E)
darEsmeraldaAlGanador g l e mpe = case lookupM g mpe of
  Nothing -> error "No existe el personaje ganador"
  Just es -> case lookupM l mpe of
    Nothing  -> error "No existe el personaje perdedor"
    Just es' -> let mpe' = assocM g (agregar e es)  mpe
                        in assocM l (sacale  e es') mpe'

ganadorYPerdedor :: Personaje -> Personaje -> (Personaje, Personaje)
ganadorYPerdedor p p' = if poder p >= poder p' then (p,p') else (p',p)

agregar :: Esmeralda -> [Esmeralda] -> [Esmeralda] -- O(E)
agregar e []      = [e]
agregar e (e':es) = if e == e' then e:es else e' : agregar e es

sacarle :: Esmeralda -> [Esmeralda] -> [Esmeralda] -- O(E)
sacarle _ []      = []
sacarle e (e':es) = if e == e' then es else e' : sacarle e es

usarEsmeralda :: EmGame → Personaje → Esmeralda → EmGame -- O(P log P + E)
usarEsmeralda (AG mpe mep hp) p e = case lookupM e mep of
  Nothing -> error "No existe la esmeralda"
  Just mp -> case mp of
    Nothing -> error "No pertenece a nadie"
    Just p' -> if p /= p'
      then error "No pertenece al personaje"
      else case lookupM p mpe of
        Nothing -> error "no existe el personaje, se rompió el inv.rep."
        Just es -> let p'   = aumentoPoderDe p (poder p)
                       mpe' = assocM p' (sacarle e es) mpe
                       mep' = deleteM e mep
                       hp'  = insertH p' (deleteH p hp)
                    in AG mpe' mep' hp'

deleteH :: Personaje -> MaxHeap Personaje -> MaxHeap Personaje -- O(P log P)
deleteH p hp = if p == findMaxH hp
               then deleteMaxH hp
               else insertH (findMaxH hp) (deleteH p (deleteMaxH hp))

data Comando = IniciarJuego (Set Personaje) (Set Esmeralda)
             | ObtenerEsmeralda Personaje Esmeralda
             | CompetirPor Personaje Personaje Esmeralda
             | UsarEsmeralda Personaje Esmeralda

partida :: [Comando] → Personaje -- O((C * P log P + E) + (P log P + E log E))
partida []     = error "No hay comandos"
partida (c:cs) = case c of
  IniciarJuego ps es -> elMasPoderoso (ejecutar (reverse cs) (iniciarJuego ps es))
  _ -> error "No es Iniciar Juego"

ejecutar :: [Comando] -> EmGame -> EmGame -- O(C * P log P + E)
ejecutar []     game = game
ejecutar (c:cs) game = case c of
  IniciarJuego     _ _    -> error "El Juego ya inició"
  ObtenerEsmeralda p e    -> obtenerEsmeralda (ejecutar cs game) p e
  CompetirPor      p p' e -> ganarEsmeralda   (ejecutar cs game) p p' e
  UsarEsmeralda    p e    -> usarEsmeralda    (ejecutar cs game) p e