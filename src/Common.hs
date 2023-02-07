module Common where
  import Data.List.NonEmpty (NonEmpty)
  -- import Control.Monad.State
  import qualified Data.Map.Strict as M

  type Name = String

  -- GrmRTerm es una gramatica regular vista de forma mas general (si es izq o der esta determinado en otra estructura)
  data GrmRTerm = GrmInitial 
                | GrmNT String
                | GrmT String
                | GLambda
                | GOr GrmRTerm GrmRTerm
                | GrmLNTT String String
                | GrmLIT String
                | LOr GrmRTerm GrmRTerm
                | GrmRTNT String String
                | GrmRTI String
                | ROr GrmRTerm GrmRTerm
                | GenRule GrmRTerm GrmRTerm
                | LeftRule GrmRTerm GrmRTerm
                | RightRule GrmRTerm GrmRTerm
                | G GrmRTerm GrmRTerm
                | L GrmRTerm GrmRTerm
                | R GrmRTerm GrmRTerm
    deriving Show

  -- la gramatica regualr puede ser izquierda o derecha
  -- la buena noticia de usar haskell es que tenemos
  -- Either que literalmente es Left 'algo' o Right 'otra cosa'
  -- asi que Left va a ser una gramatica izq y Right una gramatica der
  -- (would be very funny swap it pero se que despues voy a estar enojado conmigo mismo)
  type GrmR = Either GrmRTerm GrmRTerm

  -- por lo que estuve leyendo se que me voy a tener que meter con pasar
  -- las gramaticas a automatas asi que seguro termino necesitando guardarlas de esa forma
  -- pero esto esta completamente en borrador tengo que seguir leyendo about it
  -- simbolos automatas no deterministas
  newtype SimbND = SimbND {runSimbND :: String} 
    deriving (Eq, Ord, Show)

  -- simbolos automatas deterministas
  newtype SimbD = SimbD {runSimbD :: NonEmpty Char} -- no puedo tener empty transition en un dfa (no puedo pasar al siguiente estado usando al cadena vacia)
    deriving (Eq, Ord, Show)

  newtype State a = State {runState :: a}
    deriving (Eq, Ord, Show)

  -- relacion
  newtype Rel a = Rel [(State a, SimbND, State a)]
    deriving Show

  -- funcion
  newtype F a = F [(State a, SimbD, State a)]
    deriving Show

  data AEFD a = D [SimbD] [State a] (F a) [State a] (State a)

  data AEFND a = ND [SimbND] [State a] (Rel a) [State a] (State a)

  newtype AEFDG = AEFD (Int, Bool) -- el bool es para indicar si era izq o der. false -> izq, true -> der

  -- entorno de gramaticas con sus nombres
  -- type Env = [(Name, AEFDG)]
  type Env = M.Map Name AEFDG

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
