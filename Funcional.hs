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

--poder :: Robot -> Int
--Calcula la fuerza de un robot sumando su energía más el producto de su nivel de experiencia 
--por la cantidad de programas que tiene.
poder :: Robot -> Int
poder robot = energia robot + nivel robot * length (programas robot)

--daño :: Robot -> Programa -> Int
--Calcula cuánta energía se pierde o gana al aplicar un programa a un robot. La ganancia se 
--indica con un número negativo. La función retorna 0 si no hay cambio.
daño :: Robot -> Programa -> Int
daño robot programa
    | energia robot == energia (programa robot) = 0 
    | otherwise = energia robot - energia (programa robot)

--diferenciaDePoder :: Robot -> Robot -> Int
--La diferencia absoluta en poder entre dos robots
diferenciaDePoder :: Robot -> Robot -> Int
diferenciaDePoder robot1 robot2 = abs (poder robot1 - poder robot2)

--mejorProgramaContra :: Robot -> Robot -> Programa
--Elige el programa del segundo robot que cause mayor reducción de energía al primero.
mejorProgramaContra :: Robot -> Robot -> Programa
mejorProgramaContra objetivo atacante =foldr1 (\programa1 programa2 -> if daño objetivo programa1 >= daño objetivo programa2 then programa1 else programa2) (programas atacante)

--mejorOponente :: Robot -> Academia -> Robot
--Encuentra el robot con la mayor diferencia de poder respecto al robot recibido.
mejorOponente :: Robot -> Academia -> Robot
mejorOponente r = foldr1 (\robot1 robot2 -> if diferenciaDePoder r robot1 >= diferenciaDePoder r robot2 then robot1 else robot2)

--Implementa la función noPuedeDerrotarle :: Robot -> Robot -> Bool
--La condición es que, tras aplicar todos los programas que conoce al segundo robot, la 
--energía del primero quede igual que antes, sin necesidad de usar recursividad.
noPuedeDerrotarle :: Robot -> Robot -> Bool
noPuedeDerrotarle atacante objetivo =energia objetivo == energia (foldl (\r prog -> prog r) objetivo (programas atacante))
