module Common where

  type Name = String

  -- GrmRTerm es una gramatica regular (a priori no sabemos si izquierda o derecha)
  data GrmRTerm = GrmInitial 
                | GrmNT String
                | GrmT String

  -- la gramatica regualr puede ser izquierda o derecha
  -- la buena noticia de usar haskell es que tenemos
  -- Either que literalmente es Left 'algo' o Right 'otra cosa'
  -- asi que Left va a ser una gramatica izq y Right una gramatica der
  -- (would be very funny swap it pero se que despues voy a estar enojado conmigo mismo)
  data GrmR = Either GrmRTerm GrmRTerm

  -- por lo que estuve leyendo se que me voy a tener que meter con pasar
  -- las gramaticas a automatas asi que seguro termino necesitando guardarlas de esa forma
  -- pero esto esta completamente en borrador tengo que seguir leyendo about it
  newtype DFAG = DFA Int

  -- entorno de gramaticas con sus nombres
  type NameEnv v t = [(Name, DFAG)]

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
