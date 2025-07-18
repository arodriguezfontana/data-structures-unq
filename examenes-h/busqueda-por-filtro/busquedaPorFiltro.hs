data Busqueda = B (Map String (Map String Int))
                  [Filtro]

-- Sea (B mp fs) una busqueda.
-- * Para cada producto clave en mp, su map asociado como valor, tiene como clave "Precio" y como valor asociado un n >= 0.
-- * Para cada producto clave en mp, cada atributo de su map asociado como valor debe satisfacer a todos los filtros de fs.

-- F = cantidad de filtros de la busqueda.
-- P = cantidad de productos que tiene actualmente la busqueda
-- A = mayor cantidad de atributos de un producto en la busqueda.

registrar :: String -> Int -> Map String Int -> Busqueda -> Busqueda
registrar nom pre ma (B mp fs) = case lookupM nom mp of
                                    Nothing -> B (agregarSiSatisface nom pre ma fs mp) fs
                                    Just _ -> B mp fs

filtrar :: Filtro -> Busqueda -> Busqueda
filtrar f (B mp fs) = B (dejarLosQueSatisfacen (keysM mp) f mp) (f:fs)

siguientesN :: Busqueda -> Int -> [(String, Int)]
siguientesN b 0 = []
siguientesN b n = case fst(siguiente b) of
                    Nothing -> []
                    Just (nom, pre) -> (nom, pre) : siguientesN (lst(siguiente b)) (n-1)

agregarSiSatisface :: String -> Int -> Map String Int -> [Filtro] -> Map String Int -> Map String Int
agregarSiSatisface nom pre ma [] mp = assocM nom (atributosConPrecio pre ma) mp
agregarSiSatisface nom pre ma (f:fs) mp = if aplica f (atributosConPrecio pre ma)
                                                        then agregarSiSatisface nom pre (atributosConPrecio pre ma) fs mp
                                                        else mp

atributosConPrecio :: Int -> Map String Int -> Map String Int
atributosConPrecio pre ma = assocM "Precio" pre ma

dejarLosQueSatisfacen :: [k] -> Filtro -> Map String Int -> Map String Int
-- Precondición: Los elementos de la lista deben ser las claves pertenecientes al map.
dejarLosQueSatisfacen [] f m = m
dejarLosQueSatisfacen (p:ps) f m = let ma = (fromJust(lookupM p mp))
                                   in if aplica f ma
                                        then dejarLosQueSatisfacen ps f m
                                        else deleteM p (dejarLosQueSatisfacen ps f m)

-- siguientesN:
-- * En el peor caso, que es el camino del Just, hago:
-- * Por cada n, siendo n la cantidad de productos indicados por argumento.
-- * O(n * (fst O(1) + siguiente O(P + log P+ log A) + cons O(1) + lst(1) + siguiente O(P + log P+ log A)))
-- * Se absorben los de mismo costo y costos más pequeños.
-- * O(n * (P + log P + log A))

-- atributosConPrecio:
-- * En el peor caso hago:
-- * assocM O(log A) sobre ma.
-- * O(log A)

-- agregarSiSatisface:
-- OBS: Si se satisfacen todos los filtros, se realiza en el caso base/final.
-- * En el peor caso, que es el camino del then, hago:
-- * Por cada filtro de la busqueda F.
-- * O(F * (aplica O(log A) sobre ma + atributosConPrecio O(log A) sobre ma + atributosConPrecio O(log A) sobre ma + assocM O(log P) sobre mp)).
-- * Se absorben los iguales.
-- * O(F * (log A + log P))

-- registrar:
-- * En el peor caso, que es el caso del Nothing, hago:
-- * lookupM O(log P) sobre mp + agregarSiSatisface O(F * (log A + log P))
-- * Se absorbe el costo más pequeño.
-- * O(F * (log A + log P))

-- dejarLosQueSatisfacen:
-- En el peor caso, que es el camino del else, hago:
-- * Por cada producto P.
-- * O(P * (aplica O(log A) sobre ma + lookupM O(log P) sobre mp + deleteM O(log P) sobre mp)
-- * Se absorben los iguales.
-- * O(P * log A + log P)

-- filtrar:
-- En el peor caso hago:
-- dejarLosQueSatisfacen O(P * log A + log P) + keys O(P) sobre mp + cons O(1)
-- Se absorben los costos más pequeños.
-- O(P * log A + log P)