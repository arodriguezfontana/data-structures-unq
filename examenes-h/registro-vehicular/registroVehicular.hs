data RV = MkRV (Map CUIF Foto) 
               (Map Vehiculo (Set Vehiculo))
               
-- Sea (MkRV mcf mvv) un registro vehicular.
-- * La clave de mcf es el CUIF de la foto asignada como valor.
-- * Los vehiculos que aparecen en una foto asociada como valor en mcf, deben aparecer como clave en mvv.
-- * Los vehiculos que aparecen como clave en mvv, deben aparecer en alguna foto de mcf.
-- * Todo vehiculo que aparecen en la foto asociada como valor en mcf, debe pertenecer al conjunto asociado como valor en mvv para cada vehiculo que tambien aparezca en la foto. 
-- * Toda vehiculo que sea clave en mvv, no puede ser elemento en su conjunto asociado como valor.

-- F = cantidad total de fotos del RV.
-- V = cantidad de vehiculos diferentes que aparecen en RV.
-- vf = cantidad máxima de vehiculos en alguna foto.

nuevoRV :: RV
nuevoRV = MkRV emptyM emptyM

agregarFoto :: RV -> Foto -> RV
-- Precondición: El identificador de la foto que se agrega no fue usado previamente en el registro vehicular.  
agregarFoto (MkRV mcf mvv) f = MkRV (assocM (cifuDeFoto f) f mcf) (mConVehiculosAgregados (setToList (vehiculosDeFoto f)) (vehiculosDeFoto f) mvv)

todasLasFotos :: RV -> [CUIF] -- O(F)
todasLasFotos (MkRV mcf _) = keys mcf

quienesAparecen :: RV -> CUIF -> Set Vehiculo -- O(log F)
-- Precondición: El CUIF debe corresponder a una foto del registro. 
quienesAparecen (MkRV mcf mvv) c = case lookupM c mcf of
                                    Nothing -> error "No existe una foto con el CUIF dado."
                                    Just f -> vehiculosDeFoto f

aparecenJuntos :: RV -> Vehiculo -> Vehiculo -> Bool -- O(log V + log vf)
-- Precondición: Los vehiculos deben ser distintos. 
aparecenJuntos (MkRV _ mvv) v1 v2 = case lookupM v1 mvv of
                                        Nothing -> False
                                        Just v -> belongsS v2 v

esInnecesaria :: RV -> CUIF -> Bool
esInnecesaria rv c = isEmptyS (quienesAparecen rv c)

juntos :: Vehiculo -> Vehiculo -> RV -> Set CUIF
juntos v1 v2 rv = if aparecenJuntos rv v1 v2
                    then cifuJuntos v1 v2 rv (todasLasFotos rv)
                    else emptyS

cifuDeFoto :: Foto -> Set Vehiculo -- O(1)
cifuDeFoto (MKF c _) = c

vehiculosDeFoto :: Foto -> Set Vehiculo -- O(1)
vehiculosDeFoto (MKF _ sv) = sv

cifuJuntos :: Vehiculo -> Vehiculo -> RV -> [CIFU] -> Set CUIF
-- Precondicion: La lista tiene todos los identificadores de fotos del registro vehicular.
cifuJuntos _ _ _ [] = emptyS
cifuJuntos v1 v2 rv (c:cs) = addSiEstanJuntos c v1 v2 rv (cifuJuntos v1 v2 rv cs)

addSiEstanJuntos :: CIFU -> Vehiculo -> Vehiculo -> RV -> Set CUIF -> Set CUIF
addSiEstanJuntos c v1 v2 rv s = if aparecenAmbos c v1 v2 rv
                                    then addS c s
                                    else s

aparecenAmbos :: CIFU -> Vehiculo -> Vehiculo -> RV -> Bool
aparecenAmbos c v1 v2 rv = belongsS v1 (quienesAparecen rv c) && belongsS v2 (quienesAparecen rv c)

mConVehiculosAgregados :: [Vehiculo] -> Set Vehiculo -> Map Vehiculo (Set Vehiculo) -> Map Vehiculo (Set Vehiculo)
mConVehiculosAgregados [] _ mvv = mvv
mConVehiculosAgregados (v:vs) sv mvv = case lookupM v mvv of
                                        Nothing -> assocM v (delete v sv) (mConVehiculosAgregados vs sv mvv)
                                        Just oldS -> assocM v (union oldS (delete v sv)) (mConVehiculosAgregados vs sv mvv)

data RV = MkRV (Map CUIF Foto) 
               (Map Vehiculo (Set Vehiculo))
               Maybe CUIF

-- Sea (MkRV mcf mvv mc) un registro vehicular.
-- * Si mc es Nothing, no existen fotos.
-- * Si mc es Just v, v es el CIFU de una de las fotos con más vehiculos / el valor asociado más grande de mcf.

nuevoRVV2 :: RV
nuevoRVV2 = emptyM emptyM Nothing

masCargada :: RV -> Maybe CUIF -- O(1)
masCargada (MkRV _ _ mc) = mc

agregarFoto :: RV -> Foto -> RV                        
-- Precondición: El identificador de la foto que se agrega no fue usado previamente en el Registro.  
agregarFoto (MKRV mcf mvs) (MKF c sv) = MKRV (assocM c (MKF c sv) mcf)              
                                             (actualizarMVS (setToList sv) sv mvs) 
                                             (mciAdecuado c sv mc mcf)          

mciAdecuado :: CUIF -> Set Vehiculo -> Maybe CUIF -> Map CUIF Foto -> Maybe CUIF 
-- Precondición: El CUIF de maybe es una clave del map. 
mciAdecuado c sv mc m = case lookupM fromMaybe(mc) m of                     
                                Nothing -> error "El CUIF no pertenece al map"
                                Just f -> if sizeS sv < sizeS(vehiculosDeFoto f)
                                            then mc 
                                            else Maybe c 