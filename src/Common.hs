{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Common where

  type Name = String

  -- GramTerm es una gramatica regular vista de forma mas general (si es izq o der esta determinado en otra estructura)
  newtype GramTerm = Gram [Rule]
    deriving (Eq, Ord, Show)

  -- estructura de datos que es como GramTerm pero ademas lleva a los simbolos terminales de la gramatica
  data GramShowTerm = GramShow [T] [Rule]
    deriving (Eq, Ord, Show)

  -- segun si es izquierda o derecha
  type GramShow = Either GramShowTerm GramShowTerm

  -- una regla de produccion de una gramatica esta dada por
  -- un no terminal que seria A en A -> generacion1 | generacion2 y una lista que seria [generacion1, generacion2]
  -- en caso de tener varias reglas A -> generacion1 A -> generacion2 seran 2 reglas distintas por mas que sean del mismo no terminal
  data Rule = Rule NT [RigthSide]
    deriving (Eq, Ord, Show)

  -- no terminal
  data NT = NT String | Initial
    deriving (Eq, Ord, Show)

  -- terminal  
  type T = String

  -- en el lado derecho de una regla de produccion tenemos un terminal, un terminal y un no terminal o lambda
  data RigthSide = RT T | RTNT T NT | RL
    deriving (Eq, Ord, Show)

  -- la gramatica regular puede ser izquierda o derecha
  type Gram = Either GramTerm GramTerm

  -- simbolos automatas no deterministas
  type SimbND = String

  -- simbolos automatas deterministas
  type SimbD = String -- a tener en cuenta: no puedo tener empty transition en un dfa (no puedo pasar al siguiente estado usando la cadena vacia)

  -- estado
  type St a = a

  -- relacion de transición
  type RelT a = [(St a, SimbND, St a)]

  -- funcion de transición
  type FunT a = [(St a, SimbD, St a)]

  -- alfabeto - lista de estados - funcion de transicion - estados de aceptacion - estado inicial
  data AEFD a = D [SimbD] [St a] (FunT a) [St a] (St a) Bool  -- el bool es para indicar si era izq o der. false -> izq, true -> der
    deriving Show

  -- alfabeto - lista de estados - funcion de transicion - estados de aceptacion - estado inicial
  data AEFND a = ND [SimbND] [St a] (RelT a) [St a] (St a) Bool  -- el bool es para indicar si era izq o der. false -> izq, true -> der
    deriving Show

  -- al automata de estado finito determinista que usamos para representar la gramatica
  type AEFDG = AEFD Int

  -- entorno de gramaticas con sus nombres
  type Env = [(Name, AEFDG)]

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
