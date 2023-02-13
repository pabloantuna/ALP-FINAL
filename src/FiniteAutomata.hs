module FiniteAutomata where
import Common

import Data.Set (fromList, fold, isSubsetOf, intersection, empty, toAscList, toList, unions, Set, powerSet)
import qualified Data.Set as S (map, union, singleton)
import Data.List (union, (\\), delete, elemIndex, intersect, words, all)
import qualified Data.List.NonEmpty as NE (fromList, toList, NonEmpty)

unionAefd = undefined

intersecAefd = undefined

diffAefd = undefined

concatAefd = undefined

complementAefd = undefined

--
reverseAefnd :: AEFND a -> AEFND (Maybe a)
reverseAefnd (ND simb sts (RelT r) stsa sti b) = let sts'     = St Nothing : map (\(St x) -> St $ Just x) sts -- mis estados van a aser los mismos (llevados al nuevo tipo maybe a) y un estado nuevo Nothing
                                                     rLambda  = [(St Nothing, SimbND "", St $ Just s) | (St s) <- stsa] -- creo relaciones empty transition que vayan del nuevo estado Nothing hacia cada estado de aceptación
                                                     rReverse = [(St $ Just s2, SimbND t, St $ Just s1) | (St s1, SimbND t, St s2) <- r] -- creo relaciones a partir de las ya existentes dandolas vueltas (si antes de A->B consumiendo x ahora de B->A consumiendo x)
                                                     r'       = rLambda ++ rReverse -- mi relacion es la union de las dos relaciones creadas arriba
                                                     stsa'    = [St $ Just $ runSt sti] -- tengo un solo estado de aceptacion y es el que antes era el inicial
                                                     sti'     = St Nothing -- mi nuevo estado inicial es el estado nuevo Nothing
                                                 in ND simb sts' (RelT r') stsa' sti' b

reverseAefd = undefined

sideAefd :: AEFDG -> AEFDG
sideAefd (D s sts f stsa sti b) = D s sts f stsa sti (not b)

--
aefndToAEFD :: (Eq a, Ord a) => AEFND a -> AEFD (Set a)
aefndToAEFD (ND simb sts (RelT r) stsa sti b) =
  let simb' = map (\(SimbND x) -> SimbD $ NE.fromList x) (simb \\ [SimbND ""]) -- mi alfabato es el mismo menos la vacia, el lambda, el ""
      -- sts' = toList $ S.map St $ powerSet $ fromList $ map runSt sts --
      sts' = []
      f = []--[(u, SimbD $ NE.fromList $ runSimbND x, St $ S.singleton $ runSt s) | s <- sts, u <- sts', x <- simb, (u, x, s) `elem` r] -- [(St a,SimbD,St a)] --
      stsa' = [s | s <- sts', intersection (runSt s) (fromList (map runSt stsa)) /= empty] -- la regla de construccion del conjunto que nos dieron en 2do
      sti' = St $ fromList $ union [runSt sti] [runSt s | s <- sts, (sti, SimbND "", s) `elem` r] -- el estado inicial nuevo es el estado inicial + los estados a los cuales desde el inicial puedo hacer una lambda/empty transition
  in D simb' sts' (FunT f) stsa' sti' b