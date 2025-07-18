module Empresa( Empresa,consEmpresa,buscarPorCUIL,empleadosDelSector,todosLosCUIL,todosLosSectores,
  agregarSector,agregarEmpleado,agregarASector,borrarEmpleado
) where

import MapV1
import SetV1
import Empleado

type CUIL     = Int
type SectorId = String

data Empresa  = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)

-- INV.REP:
-- Sea (ConsE ms me):
-- * Si un empleado pertenece a ms entonces pertenece a me también.
-- * Si dos empleados son iguales por CUIL, entonces deben tener asignados los mismos sectores.

consEmpresa :: Empresa
consEmpresa = ConsE emptyM emptyM

buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL c (ConsE _ me) = case lookupM c me of
  Nothing -> error "El empleado con dicho CUIL no existe."
  Just e  -> e

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
empleadosDelSector id (ConsE ms _) = case lookupM id ms of
  Nothing -> error "El sector no existe."
  Just s  -> setToList s

todosLosCUIL :: Empresa -> [CUIL]
todosLosCUIL (ConsE _ me) = keys me

todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE ms _) = keys ms

agregarSector :: SectorId -> Empresa -> Empresa
agregarSector id (ConsE ms me) = ConsE (assocM id emptyS ms) me

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado ids c (ConsE ms me) = let newE = incorporarSectores ids (consEmpleado c) in
  ConsE (assocASectores ids newE ms) (assocM c newE me)

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
agregarASector id c (ConsE ms me) = case lookupM c me of
  Nothing -> error "El empleado no existe"
  Just e  -> let e' = incorporarSector id e in
    ConsE (assocASector id e' ms) (assocM c e' me)

borrarEmpleado :: CUIL -> Empresa -> Empresa
borrarEmpleado c (ConsE ms me) = case lookupM c me of
  Nothing -> ConsE ms me
  Just e  -> ConsE (borrarDeSectores e (sectores e) ms) (deleteM c me)

incorporarSectores :: [SectorId] -> Empleado -> Empleado
incorporarSectores []       e = e
incorporarSectores (id:ids) e = incorporarSector id (incorporarSectores ids e)

assocASectores :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
assocASectores []       _ ms = ms
assocASectores (id:ids) e ms = assocASector id e (assocASectores ids e ms)

assocASector :: SectorId -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
assocASector id e ms = case lookupM id ms of
  Nothing -> assocM id (addS e emptyS) ms
  Just s  -> assocM id (addS e s)      ms

borrarDeSectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
borrarDeSectores _ []       ms = ms
borrarDeSectores e (id:ids) ms = borrarDeSector e id (borrarDeSectores e ids ms)

borrarDeSector :: Empleado -> SectorId -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
borrarDeSector e id ms = case lookupM id ms of
  Nothing -> error "No existe el sector"
  Just s  -> assocM id (removeS e s) ms