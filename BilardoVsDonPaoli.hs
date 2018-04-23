import Text.Show.Functions

type Nombre = String
type Puntos = Int
type Tactica = String
type Titulo = (String, String)
type Torneo = (String, String)
type Accion = DT -> DT

data DT = DT { nombre :: Nombre, puntos :: Puntos, tactica :: Tactica, titulos :: [Titulo], acciones :: [Accion] } deriving (Show) 

bilardo = DT { nombre = "Carlos Salvador Bilardo", puntos = 100, tactica = "4-4-2", titulos = [("Mundial 86","Internacional"), ("Metropolitano 82", "Nacional")], acciones = [tomarGatorade, rayoBidonizador, tomarGatorade, gritar, pincharConUnAlfiler] }
donPaoli = DT "Jorge Sampaoli" 100 "3-4-1-2" [("Torneo Chileno", "Nacional"), ("Torneo Chileno", "Nacional"), ("Torneo Chileno", "Nacional"),("Sudamericana","Internacional"),("Copa America", "Internacional")] [rayoSampaolizador, gritar, pincharConUnAlfiler]

------ACCIONES--------
-- Modelo las acciones como funciones ya que su único objetivo es "transformar" al dt que las realiza (no olvidarse que en funcional no hay efecto)

-- tomarGatorade: aumenta los puntos de un personaje en 40 unidades y pone la táctica en “4-4-2”.
tomarGatorade :: Accion 
tomarGatorade = (cambiarTactica "4-4-2").(modificarPuntos 40)

modificarPuntos :: Puntos -> DT -> DT
modificarPuntos unosPuntos dt = dt { puntos = puntos dt + unosPuntos }

cambiarTactica :: Tactica -> DT -> DT
cambiarTactica unaTactica dt = dt { tactica = unaTactica }

-- pincharConUnAlfiler: suma 20 puntos y agrega gritar a la lista de acciones.
pincharConUnAlfiler :: Accion
pincharConUnAlfiler = (agregarAccion gritar).(modificarPuntos 20)

agregarAccion :: Accion -> DT -> DT
agregarAccion accion dt = dt { acciones = accion : acciones dt }

-- gritar: agrega “AHHHH” al principio del nombre.
gritar :: DT -> DT 
gritar dt = dt {nombre = "AHHH" ++ nombre dt}

-- jugarUnTorneo torneo: juega un torneo, si su táctica es “4-4-2” o “4-2-3-1”, lo gana, sino no. 
-- Ganar implica sumar 35 unidades en caso de ser nacional y 80 en caso de ser internacional (además de sumarlo a su colección de títulos).
jugarUnTorneo :: Torneo -> DT -> DT
jugarUnTorneo torneo dt
  | tieneTacticaGanadora dt = ganarTorneo torneo dt
  | otherwise = dt

tieneTacticaGanadora :: DT -> Bool
tieneTacticaGanadora (DT _ _ "4-4-2" _ _) = True
tieneTacticaGanadora (DT _ _ "4-2-3-1" _ _) = True
tieneTacticaGanadora dt = False

ganarTorneo :: Torneo -> DT -> DT
ganarTorneo (nombre, "Internacional") = sumarPuntosYAgregar (nombre, "Internacional") 80
ganarTorneo (nombre, "Nacional") = sumarPuntosYAgregar (nombre, "Nacional") 35

sumarPuntosYAgregar :: Torneo -> Puntos -> DT -> DT
sumarPuntosYAgregar torneo puntos = (agregarTorneo torneo).(modificarPuntos puntos)

agregarTorneo :: Torneo -> DT -> DT
agregarTorneo torneo dt = dt { titulos = torneo : titulos dt }
  
-- rayoBidonizador: suma 10 puntos X cada título nacional + 20 X cada título internacional.
rayoBidonizador :: DT -> DT
rayoBidonizador dt = modificarPuntos (puntosDeTitulos dt) dt

puntosDeTitulos :: DT -> Puntos
puntosDeTitulos dt = puntosNacionales dt + puntosInternacionales dt

puntosNacionales :: DT -> Puntos
puntosNacionales = torneosPorPuntaje 10 "Nacional"

puntosInternacionales :: DT -> Puntos
puntosInternacionales = torneosPorPuntaje 20 "Internacional"

torneosPorPuntaje :: Int -> String -> DT -> Puntos
torneosPorPuntaje puntaje tipoTorneo = (*puntaje).length.(titulosSegunTipo tipoTorneo)

titulosSegunTipo :: String -> DT -> [Titulo]
titulosSegunTipo tipo = filter (esDeTipo tipo).titulos 

esDeTipo :: String -> Titulo -> Bool
esDeTipo tipo (_,tipoTitulo) = tipo == tipoTitulo

-- rayoSampaolizador: resta 200 puntos salvo que su táctica sea “4-4-2”, en ese caso suma 200.
rayoSampaolizador :: DT -> DT
rayoSampaolizador (DT nombre puntos "4-4-2" titulos acciones) = (DT nombre (puntos + 200) "4-4-2" titulos acciones)
rayoSampaolizador dt = modificarPuntos (-200) dt

--------------PELEA-------------------
-- ultimoMinuto: dado un entrenador retorna una acción equivalente a todas sus acciones. 
ultimoMinuto :: DT -> Accion
ultimoMinuto = foldl1 (.).acciones

pelea :: DT -> DT -> DT
pelea unDt otroDt
  | puntosPelea unDt > puntosPelea otroDt = dtDespuesDePelear unDt
  | otherwise = dtDespuesDePelear otroDt

dtDespuesDePelear :: DT -> DT
dtDespuesDePelear dt = ultimoMinuto dt dt

puntosPelea :: DT -> Puntos
puntosPelea = puntos.dtDespuesDePelear