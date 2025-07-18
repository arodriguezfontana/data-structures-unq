data Album = MkA (Map CIFU (Set Persona)) 
                 (Map Persona (Set Persona))
                 
-- Sea (MkA mcp mpp) un album de fotos.
-- * Toda persona que sea clave en mpp, debe pretenecer a algun sp asociado como valor en mcp.
-- * Toda persona que pertenezca a algun sp asociado como valor en mcp, debe pertenecer como clave en mpp.
-- * Toda persona que pertenezca a un conjunto s1 asociado como valor en mcp, debe pertenecer al conjunto s2 asociado como valor en mpp para cada persona que tambien aparezca en s1. 
-- * Toda persona que sea clave en mpp, no puede ser elemento en su sp asociado como valor.

-- F = cantidad de fotos en el album. 
-- P = cantidad de personas distintas de las fotos del album. 
-- f = cantidad maxima de personas una foto. 

data Foto = MkF Album CIFU

agregarFoto :: Album -> CIFU -> Set Persona -> Album
agregarFoto (Mka mcp mpp) c sp = case lookupM c mcp of
                                    Nothing -> MkA (assocM c sp mcp) (mConPersonasAgregadas (setToList sp) sp mpp)
                                    Just _ -> error "Ya existe el CUIF dade el el album."

mConPersonasAgregadas :: [Persona] -> Set Persona -> Map Persona (Set Persona) -> Map Persona (Set Persona)
mConPersonasAgregadas [] _ mpp = mpp
mConPersonasAgregadas (p:ps) sp mpp = case lookupM p mpp of
                                        Nothing -> assocM p (deleteS p sp) (mConPersonasAgregadas ps sp mpp)
                                        Just oldS -> assocM p (unionS (deleteS p sp) oldS) (mConPersonasAgregadas ps sp mpp)

aparecenJuntas :: Album -> Persona -> Persona -> Bool
aparecenJuntas (MkA _ mpp) p1 p2 = case lookupM p1 mpp of
                                        Nothing -> False
                                        Just sp -> belongsS p2 sp

esPaisaje :: Foto -> Bool
esPaisaje (MkF a c) = isEmptyS (quienesAparecen a c)

siempreJuntos :: Persona -> Persona -> Album -> Set CIFU
siempreJuntos p1 p2 a = sEnLasQueAparecenJuntos p1 p2 (todasLasFotos a) a

sEnLasQueAparecenJuntos :: Persona -> Persona -> [CIFU] -> Album -> Set CIFU
sEnLasQueAparecenJuntos p1 p2 [] a = emptyS
sEnLasQueAparecenJuntos p1 p2 (c:cs) a = sAddSiAparecenJuntos p1 p2 c a (sEnLasQueAparecenJuntos p1 p2 c a)

sAddSiAparecenJuntos :: CIFU -> Vehiculo -> Vehiculo -> RV -> Set CIFU -> Set CIFU
sAddSiAparecenJuntos c p1 p2 a s = if aparecenAmbas c p1 p2 a
                                    then addS c s
                                    else s

aparecenAmbas :: CIFU -> Vehiculo -> Vehiculo -> RV -> Bool
aparecenAmbas c p1 p2 a = belongsS p1 (quienesAparecen a c) && belongsS p2 (quienesAparecen a c)

data AlbumV2 = MkA (Map CIFU (Set Persona)) 
                   (Map Persona (Set Persona))
                   (Maybe (CIFU, Int))
                 
-- Sea (MkA mcp mpp mci) un album de fotos.
-- * Si mci es Nothing, no hay fotos en mcp.
-- * Si mci es Just ci, ci es una tupla donde:
--      * Su primera componente es el CIFU con el valor asociado más grande en mcp.
--      * Su segunda componente, el tamño de dicho valor asociado.

masPopulosa :: RV -> Maybe CIFU -- O(1)
masPopulosa (MkA mcp mpp mci) = mci

agregarFoto :: Album -> CIFU -> Set Persona -> Album -- Mantiene el mismo costo.
agregarFoto (Mka mcp mpp mci) c sp = case lookupM c mcp of
                                        Nothing -> MkA (assocM c sp mcp) (mConPersonasAgregadas (setToList sp) sp mpp) (mciAdecuado mci (c, sizeS sp))
                                        Just _ -> error "Ya existe el CUIF dade el el album."

mciAdecuado :: Maybe (CIFU, Int) -> Maybe (CIFU, Int) -> Maybe (CIFU, Int) -- O(1)
mciAdecuado Nothing nMci = Just nMci
mciAdecuado (Just oMci) nMci = if snd(oMci) > snd(nMci)
                                then Just oMci
                                else Just nMci

-- agregarFoto:
-- En el peor caso, hago:
-- "lookupM" O(log F) sobre el map de todas las fotos.
-- + "assocM" O(log F) sobre el map de todas las fotos.
-- + "mConPersonasAgregadas" O(P * (log p + P log P)).
-- + "setToList" O(P) en el peor caso tiene a todas las personas del album.
-- Se eliminan constantes y se absorben los costos menores sobre el mismo elemento.
-- O(P * (log p + P log P) + log F)

-- mConPersonasAgregadas:
-- En el peor caso, por cada persona distinta del album P, hago:
-- "lookupM" O(log P) sobre el map de todas las personas.
-- + dos veces "assocM" O(log P) sobre el map de todas las personas.
-- + "unionS" O(P log P) en el peor caso tiene a todas las personas del album.
-- + "deleteS" O(log p) sobre sobre las personas de sp.
-- Se eliminan constantes y se absorben los costos menores sobre el mismo elemento.
-- O(P * (log p + P log P))

-- aparecenJuntas:
-- En el peor caso, hago:
-- "lookupM" O(log P) porque lo hago sobre el map de todas las personas.
-- + "belongS" O(log p) porque lo hago sobre las personas de sp.
-- Se eliminan constantes.
-- O(log P + log p)

-- esPaisaje:
-- En el peor caso, hago:
-- "quienesAparecen" O(log F) sobre las fotos del album.
-- + "isEmptyS" O(1).
-- El costo de "isEmptyS" se desestima por ser O(1).
-- Eficiencia: O(log F)

-- siempreJuntos:
-- En el peor caso, hago:
-- sEnLasQueAparecenJuntos O(F * (log p + log F)).
-- + todasLasFotos O(F) sobre las fotos del album.
-- Se absorben los costos menores sobre el mismo elemento.
-- Eficiencia: O(F * (log p + log F)).

-- sEnLasQueAparecenJuntos:
-- En el peor caso, por cada foto del album F, hago:
-- sAddSiAparecenJuntos O(log p + log F).
-- Eficiencia: O(F * (log p + log F)).

-- sAddSiAparecenJuntos:
-- En el peor caso hago:
-- aparecenAmbas O(log p + log F).
-- + addS (log F) sobre las fotos del album.
-- Se absorben los costos menores.
-- Eficiencia: O(log p + log F).

-- aparecenAmbas:
-- En el peor caso hago:
-- dos veces belongsS (log p) sobre las personas de la foto.
-- + dos veces quienesAparecen O(log F) sobre las fotos del album.
-- Se eliminan constantes.
-- Eficiencia: O(log p + log F).