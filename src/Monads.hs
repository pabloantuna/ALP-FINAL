module Monads where

import Common
-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
  -- Busca el valor de una variable
  lookfor :: Name -> m AEFDG
  -- Cambia el valor de una variable
  update :: Name -> AEFDG -> m ()

-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
  -- Lanza un error
  throw :: String -> m a