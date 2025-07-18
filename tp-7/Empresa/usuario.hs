comenzarCon :: [SectorId] -> [CUIL] -> Empresa
comenzarCon ids cs = agregarSectores ids (agregarEmpleados cs consEmpresa)

agregarSectores :: [SectorId] -> Empresa -> Empresa
agregarSectores []       e = e
agregarSectores (id:ids) e = agregarSector id (agregarSectores ids e)

agregarEmpleados :: [CUIL] -> Empresa -> Empresa
agregarEmpleados []     e = e
agregarEmpleados (c:cs) e = agregarEmpleado [] c (agregarEmpleados cs e)


recorteDePersonal :: Empresa -> Empresa
recorteDePersonal e = let empleados         = todosLosCUIL e
                          personalADespedir = take (div (length empleados) 2) empleados
                       in borrarEmpleados personalADespedir e

borrarEmpleados :: [CUIL] -> Empresa -> Empresa
borrarEmpleados []     e = e
borrarEmpleados (c:cs) e = borrarEmpleado c (borrarEmpleados cs e)


convertirEnComodin :: CUIL -> Empresa -> Empresa
convertirEnComodin c e = agregarASectores (todosLosSectores e) c e

agregarASectores :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarASectores []       _ e = e
agregarASectores (id:ids) c e = agregarASector id c (agregarASectores ids c e)


esComodin :: CUIL -> Empresa -> Bool
esComodin c e = estaEnSectores (buscarPorCUIL c e) (todosLosSectores e) e

estaEnSectores :: Empleado -> [SectorId] -> Empresa -> Bool
estaEnSectores _ []       _       = True
estaEnSectores e (id:ids) empresa = elem e (empleadosDelSector id empresa)
                                 && estaEnSectores e ids empresa