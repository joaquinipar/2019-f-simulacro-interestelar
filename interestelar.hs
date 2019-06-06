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
type Tiempo = Float

type Velocidad = Float

laNaveVieja tanquesDeOxigeno | tanquesDeOxigeno <6 = calcularTiempo 10
                             | otherwise = calcularTiempo 7

laNaveFuturistica _ = 0

laNaveX :: Char -> Nave -> Float
laNaveX 'A' = calcularTiempo 15
laNaveX 'B' = calcularTiempo 11
laNaveX 'C' =(*2).calcularTiempo 11 -- resolver retraso

calcularTiempo :: Float -> Nave -> Float
calcularTiempo velocidad (Nave planeta1 planeta2) = (distancia planeta1 planeta2)/velocidad

data Nave = Nave {

planetaInicial :: Planeta,
planetaFinal :: Planeta
}
type FuncionNaveEspacial = Planeta -> Planeta -> Tiempo
-- Realizar un viaje implica que el astronauta
-- aumente su edad en el tiempo de viaje correspondiente para llegar al destino elegido y cambie de planeta al mismo.

--envejecer :: Float -> Astronauta -> Astronauta


cambiarPlaneta planetaDestino astronauta = astronauta { planetaActual = planetaDestino }

viaje astronauta planetaDestino= (cambiarPlaneta planetaDestino).(envejecer (funcionNave ((planetaActual astronauta) planetaDestino)  )  astronauta
--                                Astronauta -> Astronauta  






--viaje nave planetaDestino astronauta = 
--    (
--        cambiarPlaneta planetaDestino .
--        envejecer (nave (planeta astronauta, planetaDestino))
--    ) astronauta