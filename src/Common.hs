{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Common where
  import Data.List.NonEmpty (NonEmpty)
  -- import Control.Monad.State
  -- import qualified Data.Map.Strict as M

  type Name = String

  -- GramTerm es una gramatica regular vista de forma mas general (si es izq o der esta determinado en otra estructura)
  newtype GramTerm = Gram [Rule]
    deriving Show

  -- una regla de produccion de una gramatica esta dada por
  -- un no terminal que seria A en A -> generacion1 | generacion2 y una lista que seria [generacion1, generacion2]
  data Rule = Rule NT [RigthSide]
    deriving Show

  -- no terminal
  data NT = NT String | Initial
    deriving (Eq, Ord, Show)

  -- terminal  
  newtype T = T {runT :: String}
   deriving (Eq, Ord, Show)

  data RigthSide = RT T | RTNT T NT | RL
    deriving Show

  -- la gramatica regular puede ser izquierda o derecha
  -- lo bueno de usar haskell es que tenemos
  -- Either que literalmente es Left 'algo' o Right 'otra cosa'
  -- asi que Left va a ser una gramatica izq y Right una gramatica der
  -- (would be very funny swap it pero se que despues voy a estar enojado conmigo mismo)
  type Gram = Either GramTerm GramTerm

  -- los terminales (esto inicialmente lo habia pensado como simplemente hacerlo string y no un newtype pero quiero probar asi porque se ve mas indicativo de que es)
  -- newtype T = T { runT :: String }
  --   deriving (Eq, Ord, Show)
  
  -- los no terminales (esto inicialmente lo habia pensado como simplemente hacerlo string y no un newtype pero quiero probar asi porque se ve mas indicativo de que es)
  -- newtype NT = NT { runNT :: String }
  --   deriving (Eq, Ord, Show)

  -- regla de produccion gram izquierda
  -- data RulesL = RLT NT T
  --             | RLNT NT NT T
  --             | RLL NT -- lambda, el NT este genera la cadena vacia

  -- regla de produccion gram der
  -- data RulesR = RRT NT T
  --             | RRNT NT T NT
  --             | RRL NT -- lambda, el NT este genera la cadena vacia

  -- data LGrm = LG [T] [NT] [RulesL]

  -- data RGrm = RG [T] [NT] [RulesR]

  -- type Grm = Either LGrm RGrm

  -- por lo que estuve leyendo se que me voy a tener que meter con pasar
  -- las gramaticas a automatas asi que seguro termino necesitando guardarlas de esa forma
  -- pero esto esta completamente en borrador tengo que seguir leyendo about it
  -- simbolos automatas no deterministas
  newtype SimbND = SimbND {runSimbND :: String} 
    deriving (Eq, Ord, Show)

  -- simbolos automatas deterministas
  newtype SimbD = SimbD {runSimbD :: NonEmpty Char} -- no puedo tener empty transition en un dfa (no puedo pasar al siguiente estado usando al cadena vacia)
    deriving (Eq, Ord, Show)

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

  data AEFND a = ND [SimbND] [St a] (RelT a) [St a] (St a) Bool  -- el bool es para indicar si era izq o der. false -> izq, true -> der

  type AEFDG = AEFD Int

  -- entorno de gramaticas con sus nombres
  type Env = [(Name, AEFDG)]
  -- type Env = M.Map Name AEFDG

  -- la parte de operaciones (union interseccion definicion etc)
  data Op = OpDef Name OpGram
          | OpIn String OpGram
          | OpEqual OpGram OpGram

  data OpGram = OpGram Name
              | OpUnion OpGram OpGram
              | OpIntersec OpGram OpGram
              | OpDiff OpGram OpGram
              | OpConcat OpGram OpGram
              | OpComplement OpGram
              | OpReverse OpGram
              | OpSide OpGram
