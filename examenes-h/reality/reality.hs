data Reality = R (Map Nombre Participante) (Map Juego [Nombre]) (PriorityQueue Participante) Int

-- Sea (R mp mj pp n) un Reality
-- * Para toda clave en mp, dicha clave es el respectivo nombre del participante que tiene asociado como valor.
-- * Para todo nombre N que pertenezca a una lista asociada como valor para algun juego J clave en mj, N debe aparecer como clave en mp con su respectivo participante como valor asociado.
-- * pp no acepta elementos repetidos.
-- * Para todo participante P asociado como valor a un nombre N en mp, P pertenece a PP.
-- * Para todo participante P perteneciente a pp, P aparece como valor asociado para su respectivo nombre clave en mp.
-- * n es equicalente a la cacntidad de claves asociadas en mj.
-- * Para cada nombre N que pertenezca a una lista no vacia asociada como valor para algun juego J clave en mj, el participante P como valor asociado a N en mp sabe jugar a J.
-- * Para cada participante P que aparece como valor en mp, y para cada juego J que P sabe jugar, existe al menos un nombre N tal que N está asociado a P en mp, y N aparece en la lista de nombres asociada al juego J en mj.
-- * Las listas asociadas en mj no tienen repetidos.

ingresarParticipante :: Nombre -> Reality -> Reality -- O(log p)
ingresarParticipante n (R mp mj pp cj) = case lookupM n mp of
    Just _ -> error "El participante ya existe."
    Nothing -> R (assocM n (crearP n) mp) mj (insertPQ (crearP n) pp) cj

losQueEstanAlHorno :: Reality -> [Participante] -- O(P log P)
losQueEstanAlHorno (R _ _ pp _) = lLosQueEstanAlHorno pp

lLosQueEstanAlHorno :: PriorityQueue Participante -> [Participante] -- O(P log P)
lLosQueEstanAlHorno pp = if isEmptyPQ pp
                            then []
                            else if cantJuegos(maxPQ pp) == 00
                                then maxPQ pp : (lLosQueEstanAlHorno (deleteMaxPQ pp))
                                else lLosQueEstanAlHorno (deleteMaxPQ pp)

hayClaroGanador :: Reality -> Bool -- O(1)
hayClaroGanador (R _ _ pp _) = if isEmptyPQ pp
                                then False
                                else cantJuegos(maxPQ pp) >= 5

unClaroGanador :: Reality -> Participante -- O(1)
unClaroGanador (R _ _ pp _) = if isEmptyPQ pp
                                then error "No hay participantes."
                                else if cantJuegos(maxPQ pp) >= 5
                                    then maxPQ pp
                                    else error "No hay un claro ganador."

sinElClaroGanador :: Reality -> Reality -- O(J*(P + log J))
sinElClaroGanador (R mp mj pp cj) = if isEmptyPQ pp
                                    then R mp mj pp cj
                                    else if cantJuegos(maxPQ pp) >= 5
                                        then let pg = maxPQ pp
                                                in R (deleteM (nombre pg) mp) (mJuegosSinPg pg (juegos pg) mj) (deleteMaxPQ pp) cj
                                        else error "No hay un claro ganador."

mJuegosSinPg :: Participante -> [Juego] -> Map Juego [Nombre] -> Map Juego Nombre -- O(J*(P + log J))
mJuegosSinPg pg [] mj = mj 
mJuegosSinPg pg (j:js) mj = case lookupM j mj of
                                Nothing -> error "No existe el juego."
                                Just ns -> assocM j (sacar (nombre pg) ns) (mJuegosSinPg pg js mj)

sacar :: Nombre -> [Nombre] -> [Nombre] -- O(P)
sacar n [] = [] 
sacar n (n2:ns) = if n = n2
                    then n2
                    else n2 : (sacar n ns) 

comenzarConParticipantes :: [Nombre] -> Reality -- O(P log P)
comenzarConParticipantes [] = comenzarReality
comenzarConParticipantes (n:ns) = ingresarParticipante n (comenzarConParticipantes ns)

sonTodosNovatos :: Reality -> Bool -- O(P log P)
sonTodosNovatos r = length(losQueEstanAlHorno r) == cantParticipantes Reality

losSovrevivientes :: Reality -> [Participantes] --(P*(J(P + log J)))
losSovrevivientes r = if hayClaroGanador r
                        then unClaroGanador r : (losSovrevivientes (sinElClaroGanador r))
                        else []