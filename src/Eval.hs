{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Eval
  ( evalOp, eval, evalQuery, evalQueryOp )
where

import Common
import Monads
import FiniteAutomata
import Control.Monad

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
  return $ unionAEFD og1' og2'
evalOp (OpIntersec og1 og2) = do
  og1' <- evalOp og1
  og2' <- evalOp og2
  return $ intersecAEFD og1' og2'
evalOp (OpDiff og1 og2) = do
  og1' <- evalOp og1
  og2' <- evalOp og2
  return $ diffAEFD og1' og2'
evalOp (OpConcat og1 og2) = do
  og1' <- evalOp og1
  og2' <- evalOp og2
  return $ concatAEFD og1' og2'
evalOp (OpComplement og) = do
  og' <- evalOp og
  return $ complementAEFD og'
evalOp (OpReverse og) = do
  og' <- evalOp og
  return $ reverseAEFD og'
evalOp (OpSide og) = do
  og' <- evalOp og
  return $ sideAEFD og'

eval :: Env -> OpGram -> Either String AEFDG
eval env og = case runStateError (evalOp og) env of
  Right (g, _) -> Right g
  Left x -> Left x

evalQueryOp :: (MonadState m, MonadError m) => Op -> m Bool
evalQueryOp (OpIn s og) = do
  og' <- evalOp og
  return $ inAEFD s og'
evalQueryOp (OpEqual og1 og2) = do
  og1' <- evalOp og1
  og2' <- evalOp og2
  return $ equalAEFD og1' og2'
evalQueryOp (OpDef _ _) = throw "No deberia llegar aca"


evalQuery :: Env -> Op -> Either String Bool
evalQuery env og = case runStateError (evalQueryOp og) env of
  Right (g, _) -> Right g
  Left x -> Left x