import Text.Show.Functions

type Programa = Robot -> Robot
type Academia = [Robot]

--Cada robot tiene un identificador (nombre), un nivel de experiencia, una cantidad de energía 
--y un conjunto de programas (software). A través de estos programas, un robot puede modificar 
--las capacidades de otros robots.
data Robot = Robot {
    nombre::String,
    nivel::Int,
    energia::Int,
    programas::[Programa]
} deriving (Show)

--recargarBateria: Este programa recibe un robot y lo recarga, aumentando su energía en una cantidad variable.
recargarBateria :: Int -> Programa
recargarBateria cantidad robot = robot { energia = energia robot + cantidad }

--descargaElectrica: Este programa causa una reducción de energía al robot objetivo: si su energía es mayor a 10, 
--le quita 10 puntos; en caso contrario, reduce su energía a la mitad.
descargaElectrica :: Programa
descargaElectrica robot
  | energia robot > 10 = robot { energia = energia robot - 10 }
  | otherwise          = robot { energia = div (energia robot) 2 }

--olvidarProgramas: Hace que el robot que lo recibe olvide los primeros N programas que conoce.
olvidarProgramas :: Int -> Programa
olvidarProgramas n robot = robot { programas = drop n (programas robot) }

--autoAtaque: El robot objetivo se ataca a sí mismo usando su primer programa registrado. Lanzar 
--error si no tiene ningún programa.
autoAtaque :: Programa
autoAtaque robot = (head (programas robot)) robot
