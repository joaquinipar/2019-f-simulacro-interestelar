import Text.Show.Functions

type Posicion = (Float, Float, Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

data Planeta = Planeta {

posicion :: Posicion,
dilatacion :: Float
} deriving (Show)

data Astronauta = Astronauta {

edadTerrestre :: Float,
planetaActual :: Planeta
} deriving (Show)

x109Z = Planeta {

posicion = (50,100,70),
dilatacion = 2.0
}

laTierra = Planeta {

posicion = (0,0,0),
dilatacion = 0.0
}

anastasia = Astronauta {

edadTerrestre = 22.0,
planetaActual = laTierra
}

bart = Astronauta{

edadTerrestre = 11.0,
planetaActual = x109Z
}

-- Punto 3

distanciaX :: Planeta -> Planeta -> Float
distanciaX (Planeta coordenadas1 _) (Planeta coordenadas2 _)  = (coordX coordenadas1 - coordX coordenadas2)**2

distanciaY :: Planeta -> Planeta -> Float
distanciaY (Planeta coordenadas1 _) (Planeta coordenadas2 _) = (coordY coordenadas1 - coordY coordenadas2)**2

distanciaZ :: Planeta -> Planeta -> Float
distanciaZ (Planeta coordenadas1 _) (Planeta coordenadas2 _) = (coordZ coordenadas1 - coordZ coordenadas2)**2

distancia :: Planeta -> Planeta -> Float
distancia planeta1 planeta2= sqrt(distanciaX planeta1 planeta2 + distanciaY planeta1 planeta2 + distanciaZ planeta1 planeta2 )

-- Punto 4

envejecer :: Float -> Astronauta -> Astronauta
envejecer anios astronauta= astronauta{edadTerrestre = (edadTerrestre astronauta) + anios + (dilatacionDeAstronauta astronauta) }

dilatacionDeAstronauta astronauta = dilatacion (planetaActual astronauta)

-- Punto 5
type TanquesOxigeno = Int
type Nave = (TanquesOxigeno,TipoNave)
type Tiempo = Float
type TipoNave = Char
calcularTiempo :: Float -> Planeta -> Planeta -> Tiempo
calcularTiempo velocidad planetaOrigen planetaDestino = (distancia planetaOrigen planetaDestino)/velocidad

laNaveVieja :: Astronauta -> Nave -> Planeta-> Tiempo
laNaveVieja astronauta (tanquesOxigeno,_) planetaDestino | tanquesOxigeno < 6 = calcularTiempo 10 (planetaActual astronauta) planetaDestino
                                                         | otherwise = calcularTiempo 7 (planetaActual astronauta) planetaDestino

laNaveFuturistica :: Astronauta -> Nave -> Planeta -> Tiempo
laNaveFuturistica _ (_,_) _ = 0

laNaveX :: Astronauta -> Nave -> Planeta -> Tiempo
laNaveX astronauta (tanquesOxigeno,tipoNave) planetaDestino | tipoNave == 'A' = calcularTiempo 15 (planetaActual astronauta) planetaDestino
                                              | tipoNave == 'B' = calcularTiempo 11 (planetaActual astronauta) planetaDestino
                                              | tipoNave == 'C' =  calcularTiempo 11 (planetaActual astronauta) planetaDestino

type NaveEspacial = (Astronauta -> Nave -> Planeta -> Tiempo)

viaje :: NaveEspacial -> Nave -> Planeta  -> Astronauta -> Astronauta
viaje naveEspacial (tanquesOxigeno,tipoNave) planetaDestino astronauta = astronauta {planetaActual = planetaDestino,
    edadTerrestre = (edadTerrestre astronauta) + naveEspacial astronauta nave planetaDestino}
    where nave = (tanquesOxigeno,tipoNave)

--viaje ::  NaveEspacial -> Nave -> Planeta -> Astronauta -> Astronauta
--viaje naveEspacial nave = cambiarPlaneta.(cambiarEdad naveEspacial nave)

-- cambiarPlaneta.
cambiarPlaneta :: Planeta -> Astronauta -> Astronauta
cambiarPlaneta planetaDestino astronauta = astronauta {planetaActual = planetaDestino}

cambiarEdad :: Astronauta -> NaveEspacial -> Nave -> Planeta -> Astronauta
cambiarEdad astronauta naveEspacial nave planetaDestino = astronauta {edadTerrestre = (edadTerrestre astronauta) + naveEspacial astronauta nave planetaDestino}
-- Punto 6: 

rescate ::  [Astronauta] -> NaveEspacial -> Nave -> Planeta -> Astronauta ->[Astronauta]

rescate tripulacion naveEspacial nave = (viajarJuntos tripulacion naveEspacial nave).(incorporarAlRescatado tripulacion naveEspacial nave).(ida tripulacion naveEspacial nave)
-- mandar destino y rescatado

ida :: [Astronauta] -> NaveEspacial -> Nave -> Planeta -> [Astronauta]
ida tripulacion naveEspacial (oxigeno,tipoNave) planetaDestino = map (viaje naveEspacial (oxigeno,tipoNave) planetaDestino) tripulacion

incorporarAlRescatado :: [Astronauta] -> NaveEspacial -> Nave -> Planeta -> Astronauta -> [Astronauta]
incorporarAlRescatado tripulacion naveEspacial (oxigeno,tipoNave) planetaDestino rescatado = rescatadoCambiado : tripulacion
    where rescatadoCambiado = sumarTiempo naveEspacial (oxigeno,tipoNave) planetaDestino rescatado

sumarTiempo :: NaveEspacial -> Nave -> Planeta -> Astronauta -> Astronauta
sumarTiempo naveEspacial (oxigeno,tipoNave) planetaDestino astronauta = astronauta {edadTerrestre = (edadTerrestre astronauta) + tiempoEnLlegar} 
    where tiempoEnLlegar = naveEspacial astronauta (oxigeno,tipoNave) planetaDestino

viajarJuntos :: [Astronauta] -> NaveEspacial -> Nave -> Planeta -> Astronauta -> [Astronauta]
viajarJuntos tripulacion naveEspacial (oxigeno,tipoNave) planetaDestino rescatado = map (viaje naveEspacial (oxigeno,tipoNave) laTierra) tripulacion
    
-- calcularTiempo velocidad planetaOrigen planetaDestino = (distancia planetaOrigen planetaDestino)/velocidad
-- laNaveX :: Astronauta -> Nave -> Planeta -> Tiempo
--viaje naveEspacial (tanquesOxigeno,tipoNave) planetaDestino astronauta 
