data GBBoard = GBB Coord
                   (History Coord)
                   (Map Coord (History Cell))
                   (History ChangeType)

-- Sea (GBB (n,m) hc mhc ht) un tablero de gobstones.
-- * n >= 0
-- * m >= 0
-- * Para toda coordenada c en hc, su componente x cumple x<=n y su componente y, y<=n.
-- * Para toda coordenada c clave en mhc, su componente x cumple x<=n y su componente y, y<=n.
-- * Si aun no hay cambios, ht tiene una unica historia h con el valor NoChange.
-- * Para toda historia h en hc, debe existir su correspondiente HeadChange en ht.
-- * Para toda historia h asociada como valor a una coordenada c en mhc, debe existir su correspondiente ChangeCell con el valor c en ht.
-- * Para toda historia h con el valor HeadChange en ht, debe existir su correspondiente historia en hc.
-- * Para toda historia h con el valor ChangeCell, con su coordenada c como valor en ht, c debe aparecer como clave en mch, con su correspondiente historia como valor asociado.

-- x = filas
-- y = columnas
-- C = cantidad máxima de celdas con cambios en el tablero.

emptyGBB:: Int -> Int -> GBBoard -- O(1)
emptyGBB x y = GBB (x,y) newH emptyM (register NoChange newH)

poner :: Color -> GBBoard -> GBBoard -- O(log C)
poner c (GBB t hc mhc ht) = GBB t 
                                hc
                                (mPoner c (current hc) mhc)
                                (register (ChangeCell (current hc)) ht)

mover :: Dir -> GBBoard -> GBBoard -- O(1)
mover d (GBB t hc mhc ht) = GBB t
                                (register (moverA t d (current hc)) hc)
                                mhc
                                (register HeadChange ht)

nroBolitas :: Color -> GBBoard -> Int -- O(log C) 
nroBolitas c (GBB _ hc mhc _) = case lookupM (current hc) mhc of
                                    Nothing -> 0
                                    Just hcl -> nroBolitasEn c (current hcl)

puedoMover :: Dir -> GBBoard -> Bool -- O(1)
puedoMover d (GBB t hc _ _) = puedoMoverA t d (current hc)

undo :: GGBoard -> GGBoard -- O(log C)
undo (GBB t hc mhc ht) = case current ht of
                            NoChange -> GGB t hc mhc ht
                            HeadChange -> GGB t (undo hc) mhc (undo ht)
                            ChangeCell cl -> GGB t hc (mUndo cl (current hc) mhc) (undo ht)

redo :: GGBoard -> GGBoard -- O(log C)
redo (GBB t hc mhc ht) = case current ht of
                            NoChange -> GGB t hc mhc ht
                            HeadChange -> GGB t (redo hc) mhc (redo ht)
                            ChangeCell cl -> GGB t hc (mRedo cl (current hc) mhc) (redo ht)

unaRojaEnCadaCelda :: GGBoard -> GGBoard
unaRojaEnCadaCelda b = ggbConUnaRojaEnCadaCelda (irAlBorde Oeste (irAlBorde Sur b))

ggbConUnaRojaEnCadaCelda :: GGBoard -> GGBoard
ggbConUnaRojaEnCadaCelda g = if puedoMover Este g
                                then ggbConUnaRojaEnCadaCelda (mover Este (poner Rojo g))
                                else if puedoMover Norte g 
                                     then ggbConUnaRojaEnCadaCelda (irAlBorde Oeste (mover Norte (poner Rojo g)))
                                     else poner Rojo g

irAlBorde :: Dir -> GBBoard -> GBBoard
irAlBorde d b = if puedoMover d b
                 then irAlBorde d (mover d b)
                 else b

mPoner :: Color -> Coord -> Map Coord (History Cell) -> Map Coord (History Cell)
mPoner c ccr mhc = case lookupM ccr mhc of
                    Nothing -> assocM ccr (registrar (ponerEn c (0,0,0,0)) newH) mhc
                    Just hcl -> assocM ccr (registrar (ponerEn c (current hcl)) hcl) mhc

mUndo :: Cell -> Coord -> Map Coord (History Cell) -> Map Coord (History Cell)
mUndo c ccr mhc = case lookupM ccr mhc of
                    Nothing -> error "No existe la celda."
                    Just hcl -> assocM c (undo hcl) mhc

mRedo :: Cell -> Coord -> Map Coord (History Cell) -> Map Coord (History Cell)
mRedo c ccr mhc = case lookupM ccr mhc of
                    Nothing -> error "No existe la celda."
                    Just hcl -> assocM c (redo hcl) mhc

-- unaRojaEnCadaCelda:
-- ggbConUnaRojaEnCadaCelda O(x * y * (y + log C)) + irAlBorde O(y) + irAlBorde O(x)
-- O(x * y * (y + log C))

-- ggbConUnaRojaEnCadaCelda:
-- Por cada celda del tablero C, hago:
-- puedoMover O(1) + mover O(1) + poner (log C) sobre g + irAlBorde O(y) + puedoMover O(1) + mover O(1) + poner (log C) sobre g + poner (log C) sobre g
-- O(x * y * (y + log C))

-- irAlBorde:
-- Por cada celda que recorro en la direccion dada, hago:
-- puedoMover O(1) + mover O(1)
-- O(max(x,y))