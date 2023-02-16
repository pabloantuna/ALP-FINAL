{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Common where
  import Data.List.NonEmpty (NonEmpty)
  -- import Control.Monad.State
  -- import qualified Data.Map.Strict as M

  type Name = String

  -- GramTerm es una gramatica regular vista de forma mas general (si es izq o der esta determinado en otra estructura)
  newtype GramTerm = Gram [Rule]
    deriving (Eq, Ord, Show)

  -- una regla de produccion de una gramatica esta dada por
  -- un no terminal que seria A en A -> generacion1 | generacion2 y una lista que seria [generacion1, generacion2]
  -- en caso de tener varias reglas A -> generacion1 A -> generacion2 seran 2 reglas distintas por mas que sean del mismo no terminal
  data Rule = Rule NT [RigthSide]
    deriving (Eq, Ord, Show)

  -- no terminal
  data NT = NT String | Initial
    deriving (Eq, Ord, Show)

  -- terminal  
  newtype T = T {runT :: String}
   deriving (Eq, Ord, Show)

  -- en el lado derecho de una regla de produccion tenemos un terminal, un terminal y un no terminal o lambda
  data RigthSide = RT T | RTNT T NT | RL
    deriving (Eq, Ord, Show)

  -- la gramatica regular puede ser izquierda o derecha
  -- lo bueno de usar haskell es que tenemos
  -- Either que literalmente es Left 'algo' o Right 'otra cosa'
  -- asi que Left va a ser una gramatica izq y Right una gramatica der
  -- (would be very funny swap it pero se que despues voy a estar enojado conmigo mismo)
  type Gram = Either GramTerm GramTerm

  -- por lo que estuve leyendo se que me voy a tener que meter con pasar
  -- las gramaticas a automatas asi que seguro termino necesitando guardarlas de esa forma
  -- pero esto esta completamente en borrador tengo que seguir leyendo about it
  -- simbolos automatas no deterministas
  newtype SimbND = SimbND {runSimbND :: String} 
    deriving (Eq, Ord, Show)

  -- simbolos automatas deterministas
  newtype SimbD = SimbD {runSimbD :: NonEmpty Char} -- no puedo tener empty transition en un dfa (no puedo pasar al siguiente estado usando la cadena vacia)
    deriving (Eq, Ord, Show)

  -- estado
  newtype St a = St {runSt :: a}
    deriving (Eq, Ord, Show)

  -- relacion de transición
  newtype RelT a = RelT [(St a, SimbND, St a)]
    deriving Show

  -- funcion de transición
  newtype FunT a = FunT [(St a, SimbD, St a)]
    deriving Show

  -- alfabeto - lista de estados - funcion de transicion - estados de aceptacion - estado inicial
  data AEFD a = D [SimbD] [St a] (FunT a) [St a] (St a) Bool  -- el bool es para indicar si era izq o der. false -> izq, true -> der
    deriving Show

  -- alfabeto - lista de estados - funcion de transicion - estados de aceptacion - estado inicial
  data AEFND a = ND [SimbND] [St a] (RelT a) [St a] (St a) Bool  -- el bool es para indicar si era izq o der. false -> izq, true -> der
    deriving Show

  -- al automata de estado finito determinista que usamos para representar la gramatica
  -- lo vamos a definir con un estandar donde el tipo de dato que manejan sus estados son Int por convencion y facilidad a la hora de trabajar
  -- esto nos genera algunas limitaciones porque no tenemos por ejemplo String que nos permitiria mantener los nombres de los estados tal como 
  -- se va llevando en el transcurso del programa lo cual nos serviria para mostrarle al usuario los mismo nombres que ellos ingresan (para las reglas de prod)
  -- pero como no vamos a imprimir la gramatica original que ellos ingresan si no una equivalente no importa
  type AEFDG = AEFD Int

  -- entorno de gramaticas con sus nombres
  type Env = [(Name, AEFDG)]
  -- type Env = M.Map Name AEFDG

  -- la parte de operaciones (union interseccion definicion etc)
  data Op = OpDef Name OpGram -- definicion de una gramatica (nombre = operacion entre gramaticas que ya existen o que se construyen como mas operaciones)
          | OpIn String OpGram -- consulta si una palabra pertenece al lenguaje representado por una gramatica (nuevamente la gramatica por su nombre si ya existe o como la generacion de una gramatica a traves de operaciones)
          | OpEqual OpGram OpGram -- consulta la igualdad de dos gramaticas (ya sea por nombre si ya existe o generada por operaciones)

  data OpGram = OpGram Name -- el nombre de una gramatica, se la busca en el entorno
              | OpUnion OpGram OpGram -- la union de dos gramaticas
              | OpIntersec OpGram OpGram -- la interseccion de dos gramaticas
              | OpDiff OpGram OpGram -- la resta de dos gramaticas
              | OpConcat OpGram OpGram -- la concatenacion de dos gramaticas
              | OpComplement OpGram -- el complemento de una gramatica
              | OpReverse OpGram -- el reverso de una gramatica
              | OpSide OpGram -- el cambio de lado (izq -> der, der -> izq) de una gramatica
