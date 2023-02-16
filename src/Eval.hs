{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Eval
  ( evalOp )
where

import Common
import Monads
import FiniteAutomata
import Control.Monad
-- import qualified Data.Map.Strict as M
-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either String (a, Env) }

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

instance Monad StateError where
  return x = StateError (\s -> Right (x, s))
  m >>= f =
    StateError
      (\s -> do
        (a, e) <- runStateError m s
        (b, e') <- runStateError (f a) e
        return (b, e'))

instance MonadError StateError where
  throw e = StateError (\_ -> Left e)

instance MonadState StateError where
  lookfor v = StateError $ lookfor' v
    where
      lookfor' vv s = maybe (Left ("Variable " ++ vv ++ " no definida")) (Right . (, s)) $ lookup vv s
  update v i = StateError (\s -> Right ((), update' v i s))
    where update' vu iu [] = [(vu, iu)]
          update' vu iu ((x, y):xs) = if vu == x then (vu,iu):xs else (x,y):update' vu iu xs


evalOp :: (MonadState m, MonadError m) => OpGram -> m AEFDG
evalOp (OpGram str) = lookfor str
evalOp (OpUnion og1 og2) = do
  og1' <- evalOp og1
  og2' <- evalOp og2
  return $ unionAefd og1' og2'
evalOp (OpIntersec og1 og2) = do
  og1' <- evalOp og1
  og2' <- evalOp og2
  return $ intersecAefd og1' og2'
evalOp (OpDiff og1 og2) = do
  og1' <- evalOp og1
  og2' <- evalOp og2
  return $ diffAefd og1' og2'
evalOp (OpConcat og1 og2) = do
  og1' <- evalOp og1
  og2' <- evalOp og2
  return $ concatAefd og1' og2'
evalOp (OpComplement og) = do
  og' <- evalOp og
  return $ complementAefd og'
evalOp (OpReverse og) = do
  og' <- evalOp og
  return $ reverseAefd og'
evalOp (OpSide og) = do
  og' <- evalOp og
  return $ sideAefd og'