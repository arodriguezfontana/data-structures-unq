data DualNet = DN (Switch Cliente)
                  (Map Cliente Ruta)

-- Sea (DN sc mcr) un dual net.
-- * Todo cliente clave en mcr debe pertenecer al sc, con su ruta correspondiente, que es su valor asociado en mcp.
-- * Todo cliente de sc debe ser clave de una asociacion en mcr, con su ruta correspondiente como valor asociado.

-- L = longitud de la ruta.
-- C = cantidad de clientes conectados.

emptyDN :: Dualnet -- O(1)
emptyDN = DN newSW emptyM

cantidadDeClientesConectados :: Dualnet -> Int -- O(1)
cantidadDeClientesConectados (DN _ mcr) = sizeM mcr

estaDisponible :: Ruta -> Dualnet -> Bool -- O(?)
estaDisponible r (DN sc mcr) = elem r (disponiblesADistancia sc (length r))

pinPorCliente :: Dualnet -> Heap (Int, Cliente) -- O(C * (log C + L))
pinPorCliente (DN sc mcr) = listToHeap (sizeRC (keys mcr) mcr)

listToHeap :: [(Int, Cliente)] -> Heap (Int, Cliente) -- O(C * log C)
listToHeap [] = emptyH
listToHeap ((n,c):ncs) = insertH (n,c) (listToHeap ncs)

sizeRC :: [Cliente] -> Map Cliente Ruta -> [(Int, Cliente)] -- O(C * (log C + L))
sizeRC [] m = []
sizeRC (c:cs) m = case lookupM c m of
                    Nothing -> error "No se encontro la clave."
                    Just r -> (length r, c) : sizeRC cs m

data Ruta = [Terminal]
data Cliente  = String 
data Terminal = Boca1 | Boca2
data RedPrivada a = Disponible | Conexion a

data Switch a = Terminal | Conmutador (RedPrivada a) (Switch a) (Switch a)

-- Sea (Switch c) un switch.
-- * Si un switch no tiene conexiones, es Terminal.

-- r = ruta más larga posible.

newSW :: Switch a -- O(1)
newSW = Terminal

conectar :: Ruta -> a -> Switch a -> Switch a -- 0(r)
conectar [] c sw = conectarAca c sw
conectar (t:ts) c Terminal = case t of
    Boca1 -> Conmutador Disponible (conectar ts c Terminal) Terminal
    Boca2 -> Conmutador Disponible Terminal (conectar ts c Terminal)
conectar (t:ts) c (Conmutador rp s1 s2) = case t of
    Boca1 -> Conmutador rp (conectar ts c s1) s2 
    Boca2 -> Conmutador rp s1 (conectar ts c s2)

conectarAca :: a -> Switch a -> Switch a
conectarAca c Terminal = Conmutador (Conexion c) Terminal Terminal
conectarAca c (Conmutador _ s1 s2) = Conmutador (Conexion c) s1 s2

desconectar :: Ruta -> Switch a -> Switch a -- O(r)
desconectar _ Terminal = error "No existe esa ruta dentro del switch."
desconectar [] (Conmutador _ s1 s2) = Conmutador Disponible s1 s2 
desconectar (t:ts) (Conmutador rp s1 s2) = case t of
    Boca1 -> Conmutador rp (desconectar ts s1) s2
    Boca2 -> Conmutador rp s1 (desconectar ts s2)

disponiblesADistancia :: Switch a -> Int -> [Ruta] -- O(2^N)
disponiblesADistancia Terminal _ = []
disponiblesADistancia (Conmutador rp _ _) 0 = case rp of
                                                    Disponible -> [[]]
                                                    Conexion a -> []
disponiblesADistancia (Conmutador _ s1 s2) n = consACada Boca1 (disponiblesADistancia s1 (n-1)) ++  
                                               consACada Boca2 (disponiblesADistancia s2 (n-1))

consACada :: a -> [[a]] -> [[a]] -- O(N)
consACada x [] = []
consACada x (ys:yss) = x : ys : consACada x yss