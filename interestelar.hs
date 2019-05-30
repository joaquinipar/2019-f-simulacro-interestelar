import Text.Show.Functions

type Posicion = (Float, Float, Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

-- Punto 1

data Planeta = Planeta{

posicion :: Posicion,
dilatacion :: Float
} 

data Astronauta = Astronauta{

edadTerrestre :: Float,
planetaActual :: Planeta
}

-- Punto 2: 

x109z = Planeta {
    posicion = (50,100,70),
    dilatacion = 2.0
}
laTierra = Planeta{
    posicion = (0,0,0)
    dilatacion = 0.0
}

anastasia = Astronauta {
    edadTerrestre = 22.0
    planetaActual = laTierra
}

bart = Astronauta{
    edadTerrestre = 11.0,
    planetaActual = x109z
}

-- Punto 3

distancia :: Planeta -> Planeta -> Float
distancia = sqrt.(**2).

resta planeta1 planeta2 = 
