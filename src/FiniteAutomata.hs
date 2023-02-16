{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module FiniteAutomata where
import Common

import Data.Set (fromList, fold, isSubsetOf, empty, intersection, Set)
import qualified Data.Set as S (map, union)
import Data.List (union, (\\), elemIndex, nub)
import qualified Data.List.NonEmpty as NE (fromList)
import Data.Maybe (fromJust)

unionAefd :: a
unionAefd = undefined

intersecAefd :: a
intersecAefd = undefined

diffAefd :: a
diffAefd = undefined

concatAefd :: a
concatAefd = undefined

complementAefd :: a
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

reverseAefd :: a
reverseAefd = undefined

sideAefd :: AEFDG -> AEFDG
sideAefd (D s sts f stsa sti b) = D s sts f stsa sti (not b)

-- funcion que toma un estado, una lista de estados y una lista de relaciones
-- y devuelve una lista de todos los estados
-- que son alcanzables a traves de lambda transitions desde el estado tomado como primer arg
lambdaReach :: (Eq a) => St a -> [St a] -> [(St a, SimbND, St a)] -> [St a]
lambdaReach st sts r = let vecinosLambdaDirectos = lambdaReach' (sts \\ [st]) -- no me interesa alcanzarme a mi mismo a traves de lambda transitions asi que me saco de los estados a chequear
                           sts' = sts \\ (st:vecinosLambdaDirectos) -- los estados que me interesan ahora son los que no alcance de forma directa ni soy yo
                           vecinosLambdaIndirectos = concatMap (\x -> lambdaReach x sts' r) vecinosLambdaDirectos -- calculo los vecinos lambda de mis vecinos lambdas recursivamente
                       in nub $ vecinosLambdaDirectos ++ vecinosLambdaIndirectos -- uno los resultados y saco repetidos
  where lambdaReach' [] = [] -- caso base trivial
        lambdaReach' (x:xs) | (st, SimbND "", x) `elem` r = x:lambdaReach' xs -- si tengo una lambda transition de st a x tomo ese x y lo agrego a la lista que construyo recursivamente viendo a que otros llego de forma directa con lambda transitions y a cuales llego a traves de ese x luego saco repetidos
                            | otherwise                   = lambdaReach' xs -- si no llego a ese x con una lambda transition entonces sigo viendo a que otros estados llego

--------------------------------------------------------------------
--- Toda esta parte es horrible y confusa y me gustaria buscar
--- otra forma de hacerlo pero no me da mas la cabeza
--- voy a intentar comentarlo de la forma mas comprensible posible
--- pero la verdad es que por mas que no se me ocurra otra cosa
--- lo siento muy como atado con alambre asi que TODO: mejorar el pasaje a AEFD
--------------------------------------------------------------------

-- esta funcion toma los simbolos del no determinista
-- los estados originales del no determinista
-- la relacion original del no determinista
-- la funcion/relacion de transicion hibrida obtenida en el ultimo paso
-- la funcion/relacion de transicion hibrida acumulada hasta el momento
-- se calcula las nuevas relaciones en base a las conseguidas en el ultimo paso
-- repitiendo el proceso de mapear sobre los simbolos y buscar con cada simbolo
-- que nuevo estado (conjunto de estados originales) alcanzo desde un estado (conjunto de estados originales)
-- usando la funcion allReach' descrita abajo
-- ese mapeo se realiza dentro de otro mapeo que aplicamos a la relacion/funcion hibrida obtenida en el ultimo paso
-- tomamos el estado (conjunto de estados originales) al cual se llega en cada uno de los elementos de esa relacion
-- y ese estado lo utilizamos en la funcion allReach' usada en el mapeo interno para determinar a
-- que nuevo estado lleva cada simbolo desde ese estado tomado
-- si la relacion/funcion obtenida ya esta contenido en la relacion/funcion total entonces ya termine esa parte de la iteracion corto la recursion
-- devolviendo la funcion/relacion hibrida acumulada hasta el momento, si no la calculada actualmente pasa a ser la ultima y es agregada a la acumulacion
-- en una nueva llamada recursiva
allReach'' :: (Ord a) => [SimbND] -> [St a] -> [(St a, SimbND, St a)] -> [(St (Set a), SimbND, St (Set a))] -> [(St (Set a), SimbND, St (Set a))] -> [(St (Set a), SimbND, St (Set a))]
allReach'' simbs sts r fLast fAcum = let fNow = concatMap (\(_, _, s') -> map (\x -> (s', x, St $ allReach' (runSt s') x sts r)) simbs) fLast -- calculo como seria la relacion/funcion (es un hibrido en este paso) de transicion de este paso (i.e los que alcanzaría en esta iteracion)
                                    in (if isSubsetOf (fromList fNow) (fromList fAcum) then fAcum else allReach'' simbs sts r fNow (nub $ fNow ++ fAcum)) -- si los que calcule en esta iteración ya son un subconjunto de los que tenia calculado entonces ya termine y devuelvo los acumulado (i.e si estoy calculando estados que ya tengo entonces ya esta) si no agrego mis estados a los acumulados y recursivamente busco los que siguen a los que calcule en esta iteracion

-- esta funcion toma un estado del determinista (conjunto de estados) un simbolo no determinista
-- los estados originales del no determinista y la relacion del no determinista
-- y va a devolver un conjunto de estados que sera el estado alcanzado por el estado
-- pasado como primer argumento. Para calcular esto nuevamente recurrimos al metodo
-- dado en LFyC, por cada estado de mi conjunto de estado me fijo a que estados (en el no determinista)
-- puedo alcanzar con cada simbolo (y con lambdaTransitions desde esos nuevos estados, 
-- i.e S -> A consumiendo "a" pero luego de A puede ir a B sin consumir nada entonces
-- {S} puede ir a A consumiendo "a" pero también puede ir a B consumiendo "a"
-- por lo que tendriamos {S} -> {A} consumiendo "a")
-- Luego de obtener los estados van a estar como una lista de Set entonces lo foldeo con la union
-- y obtengo efectivamente el nuevo estado determinista (conjunto de estados del no determinista)
-- al cual puedo ir desde mi estado st pasado como arg
allReach' :: (Ord a) => Set a -> SimbND -> [St a] -> [(St a, SimbND, St a)] -> Set a
allReach' st x sts r = fold S.union empty $ S.map reach st -- foldeo la union de conjuntos con el conjunto vacio como elemento base en la lista de conjuntos que obtengo
  where reach s = let vecinosDirectos = fromList [runSt s' | s' <- sts, (St s, x, s') `elem` r] -- calculo mis vecinos directos a los que puedo hacer una transicion con un simbolo (no lambda transition)
                  in S.union vecinosDirectos (allReach' vecinosDirectos (SimbND "") sts r) -- hago la union de todo lo que alcanzo de forma directa con los simbolos + los estados alcanzados por lambda transition de esos que alcance de forma directa. Otra opcion para esto seria mapear la funcion lambdaReach pero es medio como que peor en eficiencia: (fromList $ concatMap (\estadito -> map runSt $ lambdaReach (St estadito) sts r) (toList vecinosDirectos))

-- funcion que va a tomar el estado inicial del determinista
-- los simbolos del no determinista, los estados del no determinista
-- y la relacion original del no determinista
-- y va a devolver la funcion de transición calculada de la siguiente forma
-- primero mapeamos a una nueva funcion/relacion (es medio hibrido porque sigue usando simbolos no deterministas pero ya tiene estados como Set) f
-- para ese mapeo lo que hacemos es mapear sobre todos los simbolos no deterministas una funcion
-- que construira la relacion (estadoInicial, simbolo, estado (conjunto de estados originales) que alcanzo con ese simbolo desde el estado inicial)
-- para calcular los estados (3er elemento de la 3-upla mencionada) hago uso de una funcion auxiliar allReach' que esta inmediatamente arriba y descripta ahi
-- luego hago uso de otra funcion auxiliar allReach'' la cual me va a devolver la funcion/relacion hibrida completa
-- por ultimo la mapeo al tipo de funcion de transicion cambiando los simbolos 
allReach :: (Ord a) => St (Set a) -> [SimbND] -> [St a] -> [(St a, SimbND, St a)] -> [(St (Set a), SimbD, St (Set a))]
allReach sti simbs sts r = map (\(s, SimbND x, s') -> (s, SimbD $ NE.fromList x, s')) $ allReach'' simbs sts r f f -- usando la f descripta abajo hago uso de una funcion cuyo objetivo sera devolver la relacion en su totalidad tomando la relacion construida hasta ahora. Luego mapeo esa lista a la forma de funcion de transicion
                           where f = map (\s -> (sti, s, St $ allReach' (runSt sti) s sts r)) simbs -- mapeo sombre todos los simbolos una funcion para generar una transicion del estado inical pasado como arg hacia un conjunto de estados por cada simbolo (el conjunto de estados obtenido serían todos los estados del no determinista a los cuales llego con ese simbolo desde cada uno de los estados de mi conjunto de estados inicial en el no determinista, i.e me fijo en el no determinista tomando cada simbolo a que estados voy desde los estados que esten en el nuevo estado inicial del determinista)

-- funcion que dado un automata no determinista construye un
-- automata determinista equivalente. Usamos Set siguiendo
-- la idea de construirlo dada en la materia LFyC de 2do año
-- Se debe tener en cuenta que:
-- - Los nuevos estados son conjuntos de los estados del no determinista
-- -Entre los simbolos ya no puede estar la cadena vacia
-- -Si un estado (conjunto de estados originales) incluye un estado original de aceptacion
-- entonces este nuevo estado tambien es de aceptacion
-- -El estado inicial es el estado inicial original + los estados que pueden ser alcanzados a partir de 
-- este estado original en el no determinista a traves de lambda transitions
aefndToAEFD :: (Ord a) => AEFND a -> AEFD (Set a)
aefndToAEFD (ND simb sts (RelT r) stsa sti b) =
  let simb' = map (\(SimbND x) -> SimbD $ NE.fromList x) (simb \\ [SimbND ""]) -- mi alfabato es el mismo menos la vacia, el lambda, el ""
      sti' = St $ fromList $ union [runSt sti] [runSt s | s <- lambdaReach sti sts r] -- el estado inicial nuevo es el estado inicial + los estados a los cuales puedo llegar desde el inicial a traves de lambda/empty transitions
      f = allReach sti' (simb \\ [SimbND ""]) sts r -- obtengo la funcion de transicion 
      sts' = nub $ map (\(s, _, _) -> s) f ++ map (\(_, _, s') -> s') f -- obtengo los estados a partir de la funcion de transición y elimino repetidos (esto creo que de alguna forma podria hacerlo al mismo tiempo que calculo las funciones pero se me esta rompiendo la cabeza a esta altura)
      stsa' = [s | s <- sts', intersection (runSt s) (fromList (map runSt stsa)) /= empty] -- la regla de construccion del conjunto que nos dieron en 2do
  in D simb' sts' (FunT f) stsa' sti' b

-- funcion para pasar de un automata determinista de estados de tipo a
-- a un automata determinista de estados de tipo Int
-- esto es asi para tener alguna convencion y poder trabajar
-- a los automatas como el mismo tipo de dato
aefdToAEFDG :: (Eq a) => AEFD a -> AEFDG
aefdToAEFDG (D s sts (FunT f) stsa sti b) = let indexes = [0..length sts - 1] -- construyo una lista de 0 a ultimo indice necesario para representar mi cantidad de estados como ints
                                                stateRename st = St $ fromJust $ elemIndex st sts -- una funcion trivial que simplemente renombra un estado de tipo `a` a tipo Int
                                                sts' = map St indexes -- mis nuevos estados son los indices que consegui arriba asi que simplemente los mapeo con el constructor de estados para tener el tipo correcto
                                                f' = map (\(st, t, st') -> (stateRename st, t, stateRename st')) f -- mi nueva funcion de transicion es la misma pero con los nombres de los estados cambiados por la funcion descripta anteriormente stateRename
                                                stsa' = map stateRename stsa -- al igual que la funcion de transicion, mis estados de aceptacion son los mismos solo que los llamo con Ints usando stateRename
                                                sti' = stateRename sti -- igual que lo anterior, mismo estado solo que lo renombro a Int con stateRename
                                            in D s sts' (FunT f') stsa' sti' b