import Text.Show.Functions

type Posicion = (Float, Float, Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

data Planeta = Planeta {

posicion :: Posicion,
dilatacion :: Float
}

data Astronauta = Astronauta {

edadTerrestre :: Float,
planetaActual :: Planeta
}

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

pasarAnios :: Astronauta -> Float -> Astronauta
pasarAnios astronauta anios= astronauta{edadTerrestre = (edadTerrestre astronauta) + anios + (dilatacionDeAstronauta astronauta) }

dilatacionDeAstronauta astronauta = dilatacion (planetaActual astronauta)

-- Punto 5
type Tiempo = Float

type Velocidad = Float

data Nave = Nave {

planetaOrigen :: Planeta,
planetaDestino :: Planeta,
tanquesOxigeno :: Float,
tipoX :: Char
}

laNaveVieja :: Nave -> Float
laNaveVieja nave | (tanquesOxigeno nave) < 6 = 10
                 | otherwise = 7

laNaveFuturista _ = 0

--laNaveX :: 
laNaveX nave | tipoX == 'a' = 15
             | tipoX == 'b' = 11
             | otherwise = 11  -- agregar retraso

tiempo :: (Nave -> Float)-> Nave -> Tiempo
tiempo funcion nave velocidad= distancia (planetaOrigen nave) (planetaDestino nave)/
