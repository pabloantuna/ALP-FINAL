module FiniteAutomata where
import Common
import Data.List.NonEmpty (NonEmpty)

unionAefd = undefined

intersecAefd = undefined

diffAefd = undefined

concatAefd = undefined

complementAefd = undefined

reverseAefd = undefined

sideAefd :: AEFDG -> AEFDG
sideAefd (D s sts f stsa sti b) = D s sts f stsa sti (not b)

-- aefndToAEFD :: AEFND a -> AEFD [a]
-- aefndToAEFD (ND a sts (RelT r) stsa sti b) = 
--   let a' = 
--       sts' =
--       f =
--       stsa' = 
--       sti' = St ([runSt sti] ++ [runSt s | s <- sts, elem (sti, "", s) r]) -- el estado inicial nuevo es el estado inicial + los estados a los cuales desde el inicial puedo hacer una lambda/empty transition. Falta ver tema repetido tal vez?
--    in D a' sts' (FunT f) stsa' sti' b

aefndToAEFD = undefined