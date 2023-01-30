module Common where

  type Name = String

  -- data Gram = 
  --           |

  newtype DFAG = DFA Int

  -- entorno de gramaticas con sus nombres
  type NameEnv v t = [(Name, DFAG)]

  data Op = OpDef Name OpGram
          | OpIn String OpGram
          | OpEqual OpGram OpGram

  data OpGram = Gram Name
              | OpUnion OpGram OpGram
              | OpIntersec OpGram OpGram
              | OpDiff OpGram OpGram
              | OpConcat OpGram OpGram
              | OpComplement OpGram
              | OpReverse OpGram
              | OpSide OpGram
