{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module FiniteAutomata where
import Common

import Data.Set (fromList, fold, isSubsetOf, empty, intersection, Set)
import qualified Data.Set as S (map, union)
import Data.List (union, (\\), elemIndex, nub, intersect)
import qualified Data.List.NonEmpty as NE (fromList, singleton, toList)
import Data.Maybe (fromJust)


------------
--  Las operaciones de automatas
------------

-- funcion que dado un automata determinista y un conjunto
-- de simbolos deterministas los agrega al automata
-- creando un nuevo estado basura y las transiciones correspondientes
-- de todos los estados con los nuevos simbolos al estado basura
agregarSimbDs :: AEFD a -> [SimbD] -> AEFD (Maybe a)
agregarSimbDs (D simb sts (FunT f) stsa sti b) simbsNuevos = let simb' = nub $ simb ++ simbsNuevos
                                                                 sts' = St Nothing:map (St . Just . runSt) sts
                                                                 f' = map (\(st, x, st') -> (St $ Just $ runSt st, x, St $ Just $ runSt st')) f -- el mapeo a maybe de las transiciones existentes
                                                                  ++ [(st, x, St Nothing) | st <- sts', x <- simbsNuevos] -- las transiciones basura de los nuevos simbolos
                                                                 stsa' = map (St . Just . runSt) stsa
                                                                 sti' = St $ Just $ runSt sti
                                                             in D simb' sts' (FunT f') stsa' sti' b

-- funcion auxiliar que realiza lo que seria en si la union de los dos
-- automatas pero que tiene en cuenta que ya se realizo una
-- union de los conjuntos de simbolos por lo cual los automatas
-- que le llegan a ella ya presentan el mismo conjunto de simbolos
-- que sera parte del resultado de la union y los estados y transiciones
-- correspondientes a todo el conjunto (i.e ya pasaron por la funcion sameSimbs)
-- notar que la unica diferencia con la interseccion es cuales son los estados de aceptación
unionAEFD' :: (Eq a, Eq b) => AEFD a -> AEFD b -> AEFD (a, b)
unionAEFD' (D simb sts (FunT f) stsa sti b) (D _ sts' (FunT f') stsa' sti' b') = let sti'' = St (runSt sti, runSt sti')
                                                                                     sts'' = [St (runSt st, runSt st') | st <- sts, st' <- sts']
                                                                                     f'' = [(St (q1, q3), x, St (q2, q4)) | x <- simb, St (q1, q3) <- sts'', St (q2, q4) <- sts'', (St q1, x, St q2) `elem` f, (St q3, x, St q4) `elem` f']
                                                                                     stsa'' = [St (q1, q2) | St (q1, q2) <- sts'', elem (St q1) stsa || elem (St q2) stsa']
                                                                                     b'' = b && b'
                                                                                 in D simb sts'' (FunT f'') stsa'' sti'' b''

-- funcion que dado dos dfa devuelve otros dos con el mismo conjunto de simbolos
-- agregandole a cada uno los simbolos que no tengan del otro y las transiciones 
-- y modificaciones a estados y estados de aceptación correspondientes
sameSimbs :: Eq a => AEFD a -> AEFD a -> (AEFDG, AEFDG)
sameSimbs aefd@(D simb _ _ _ _ _) aefd'@(D simb' _ _ _ _ _) =
  let simbFaltantes = simb' \\ simb
      simbFaltantes' = simb \\ simb'
      aefdn = aefdToAEFDG $ agregarSimbDs aefd simbFaltantes
      aefdn' = aefdToAEFDG $ agregarSimbDs aefd' simbFaltantes'
  in (aefdn, aefdn')

-- funcion que realiza la union de dos automatas deterministas
-- generando primero dos nuevos automatas iguales a los dados pero con
-- los simbolos igualados, luego se genera la union y por ultimo
-- se lo lleva a la forma de AEFDG es decir con sus estados como Int
unionAEFD :: AEFDG -> AEFDG -> AEFDG
unionAEFD aefd aefd' = let (aefdn, aefdn') = sameSimbs aefd aefd'
                       in aefdToAEFDG $ unionAEFD' aefdn aefdn'

-- funcion auxiliar que realiza lo que seria en si la intersección de los dos
-- automatas pero que tiene en cuenta que ya se realizo una
-- union de los conjuntos de simbolos por lo cual los automatas
-- que le llegan a ella ya presentan el mismo conjunto de simbolos
-- que sera parte del resultado de la union y los estados y transiciones
-- correspondientes a todo el conjunto (i.e ya pasaron por la funcion sameSimbs)
-- notar que la unica diferencia con la union es cuales son los estados de aceptación
intersecAEFD' :: (Eq a, Eq b) => AEFD a -> AEFD b -> AEFD (a, b)
intersecAEFD' (D simb sts (FunT f) stsa sti b) (D _ sts' (FunT f') stsa' sti' b') = let sti'' = St (runSt sti, runSt sti')
                                                                                        sts'' = [St (runSt st, runSt st') | st <- sts, st' <- sts']
                                                                                        f'' = [(St (q1, q3), x, St (q2, q4)) | x <- simb, St (q1, q3) <- sts'', St (q2, q4) <- sts'', (St q1, x, St q2) `elem` f, (St q3, x, St q4) `elem` f']
                                                                                        stsa'' = [St (q1, q2) | St (q1, q2) <- sts'', St q1 `elem` stsa, St q2 `elem` stsa']
                                                                                        b'' = b && b'
                                                                                    in D simb sts'' (FunT f'') stsa'' sti'' b''

-- funcion que realiza la interseccion de dos automatas deterministas
-- generando primero dos nuevos automatas iguales a los dados pero con
-- los simbolos igualados, luego se genera la interseccion y por ultimo
-- se lo lleva a la forma de AEFDG es decir con sus estados como Int
intersecAEFD :: AEFDG -> AEFDG -> AEFDG
intersecAEFD aefd aefd' = let (aefdn, aefdn') = sameSimbs aefd aefd'
                          in aefdToAEFDG $ intersecAEFD' aefdn aefdn'

-- funcion que realiza la resta de dos automatas deterministas
-- para realizar la resta primero hacemos lo mismo que en la union
-- y la intersección con los simbolos de cada automata
-- y los igualamos usando sameSimbs
-- Luego hacemos uso de la intersección y el complemento para calcular la resta en si
diffAEFD :: AEFDG -> AEFDG -> AEFDG
diffAEFD aefd aefd' = let (aefdn, aefdn') = sameSimbs aefd aefd' in intersecAEFD aefdn (complementAEFD aefdn')

-- funcion que toma dos automatas deterministas y devuelve
-- otro que representa el lenguaje de la concatenacion de
-- los lenguajes de los automatas dados
-- Como para hacer la concatenacion vamos a crear lambda transitions
-- desde los estados de aceptacion del primero al estado inicial del segundo
-- lo que haremos sera pasar ambos automatas deterministas a no deterministas
-- realizar la concatenacion de esa forma y luego pasarlo a determinista nuevamente
-- para finalmente estandarizarlo a AEFDG
concatAEFD :: AEFDG -> AEFDG -> AEFDG
concatAEFD aefd aefd' = let aefnd  = aefdToAEFND aefd
                            aefnd' = aefdToAEFND aefd'
                        in aefdToAEFDG $ aefndToAEFD $ concatAEFND aefnd aefnd'

-- funcion que toma un automata determinista y devuelve
-- otro que representa el lenguaje del complemento del
-- lenguaje del automata dado. Esto es cambiar los estados
-- de aceptación para que sean los que no son de aceptación
-- en el automata dado como argumento a la funcion
complementAEFD :: AEFDG -> AEFDG
complementAEFD (D simb sts (FunT f) stsa sti b) = let stsa' = [st | st <- sts, st `notElem` stsa]
                                                  in D simb sts (FunT f) stsa' sti b

-- funcion que dado un automata determinista
-- devuelve el automata determinista que acepta el lenguaje
-- reverso de la gramatica que representa el automata dado
-- para esto pasamos el automata determinista a no determinista
-- utilizamos luego la funcion de reverseAEFND que ya hicimos 
-- anteriormente. Luego lo volvemos a pasar a AEFD
-- y por ultimo estandarizamos a AEFDG
reverseAEFD :: AEFDG -> AEFDG
reverseAEFD = aefdToAEFDG . aefndToAEFD . reverseAEFND . aefdToAEFND

-- funcion que toma un automata y devuelve el mismo pero
-- cambiando el valor del booleano guardado que indica
-- si la gramatica original era a izquierda o a derecha
-- generando asi que ahora la gramatica original sea el contrario
-- OBS: esta operacion debido a la forma de guardar las gramaticas
-- como automatas resulta bastante inutil pero agregamos la opcion
-- de imprimir una gramatica sin indicar si la queremos
-- imprimir a derecha o a izquierda y de esta forma esto tiene un uso
-- que es: se va a mostrar la gramatica segun si el booleano indica derecha o izquierda
sideAEFD :: AEFDG -> AEFDG
sideAEFD (D simb sts f stsa sti b) = D simb sts f stsa sti (not b)

----------
-- Las operaciones booleanas
----------

-- funcion que dado un string, un automata DETERMINISTA, y un
-- estado inicial determina si desde dicho estado puede
-- formar la palabra dada en el string 
-- True -> el lenguaje acepta desde ese estado la palabra dada
-- False -> el lenguaje no acepta la palabra dada desde el estado dado
acceptFromSt :: Eq a => String -> AEFD a -> St a -> Bool
acceptFromSt [] (D _ _ _ stsa _ _) st = st `elem` stsa -- cuando ya no tengo simbolos que consumir entonces me fijo si el estado en el cual estoy es de aceptacion, si es asi, la palabra es aceptada si no no
acceptFromSt (x:xs) aefd@(D _ sts (FunT f) _ _ _) st = let st' = head [s | s <- sts, (st, SimbD $ NE.singleton x, s) `elem` f] -- lista de a lo sumo un solo elemento (debido al determinismo del automata) donde ese elemento es el siguiente estado al cual voy consumiendo el simbolo actual (Obs: esta lista nunca sera vacia porque ya se chequeo que todos los simbolos pertenezcan a mi alfabeto por lo cual siempre voy a algun estado aunque sea el estado basura)
                                                       in acceptFromSt xs aefd st' -- llamada recursiva con el resto de caracteres a consumir y el estado al cual debo ir ahora

-- Funcion que dado un string y un conjunto de simbolos pertenecientes
-- a un automata determinista determina si todos los simbolos de la string
-- pertenecen al conjunto de simbolos del automata
simbolosValidos :: String -> [SimbD] -> Bool
simbolosValidos s simb = all (\c -> SimbD (NE.singleton c) `elem` simb) s

-- funcion que dada una palabra y un automata
-- determina si dicha palabra pertenece al lenguaje representado por el automata
-- obs: palabra en el sentido de palabra de un lenguaje, por ejemplo si el lenguaje acepta el caracter ' ' (espacio) entonces
-- "hola mundo" cuenta como una palabra y no dos
inAEFD :: String -> AEFDG -> Bool
inAEFD w aefd@(D simb _ _ _ sti _) = simbolosValidos w simb && acceptFromSt w aefd sti

equalAEFD :: AEFDG -> AEFDG -> Bool
equalAEFD = undefined

---------
-- Minimizar automata determinista
---------

removeUnreachable :: Eq a => AEFD a -> AEFD a
removeUnreachable aefd@(D simb sts (FunT f) stsa sti b) = let sts' = reachableStates aefd [sti]
                                                              f' = filter (\(st, _, st') -> st `elem` sts' && st' `elem` sts') f
                                                              stsa' = intersect stsa sts'
                                                          in D simb sts' (FunT f') stsa' sti b

reachableStates :: AEFD a -> [St a] -> [St a]
reachableStates aefd@(D simb sts (FunT f) stsa sti b) rsts = undefined
  -- let newSts = 
      
  -- in if algo then rsts else reachableStates aefd 

minimizeAEFD :: AEFD a -> AEFD a
minimizeAEFD = undefined

---------
-- Utilidad varios
---------

-- funcion que dado un automata no determinista
-- devuelve el automata no determinista que acepta el lenguaje
-- reverso de la gramatica que representa el automata dado
-- utilizaremos nuevamente el tipo de dato Maybe para generar un estado extra
-- que en este caso será un nuevo estado inicial del cual
-- tenemos empty transitions a todos los estados de aceptación del automata dado
-- debido a que podemos tener varios estados de aceptación en el automata original
-- que ahora deben comportarse como estados iniciales, pero como el estado inicial
-- debe ser uno solo lo resolvemos permitiendo ir a cualquiera de esos estados
-- desde un nuevo estado inicial que solo existe para dicha funcion (el estado Nothing)
reverseAEFND :: AEFND a -> AEFND (Maybe a)
reverseAEFND (ND simb sts (RelT r) stsa sti b) = let sts'     = St Nothing : map (\(St x) -> St $ Just x) sts -- mis estados van a aser los mismos (llevados al nuevo tipo maybe a) y un estado nuevo Nothing
                                                     rLambda  = [(St Nothing, SimbND "", St $ Just s) | (St s) <- stsa] -- creo relaciones empty transition que vayan del nuevo estado Nothing hacia cada estado de aceptación
                                                     rReverse = [(St $ Just s2, SimbND t, St $ Just s1) | (St s1, SimbND t, St s2) <- r] -- creo relaciones a partir de las ya existentes dandolas vueltas (si antes de A->B consumiendo x ahora de B->A consumiendo x)
                                                     r'       = rLambda ++ rReverse -- mi relacion es la union de las dos relaciones creadas arriba
                                                     stsa'    = [St $ Just $ runSt sti] -- tengo un solo estado de aceptacion y es el que antes era el inicial
                                                     sti'     = St Nothing -- mi nuevo estado inicial es el estado nuevo Nothing
                                                 in ND simb sts' (RelT r') stsa' sti' b

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
aefdToAEFDG (D simb sts (FunT f) stsa sti b) = let indexes = [0..length sts - 1] -- construyo una lista de 0 a ultimo indice necesario para representar mi cantidad de estados como ints
                                                   stateRename st = St $ fromJust $ elemIndex st sts -- una funcion trivial que simplemente renombra un estado de tipo `a` a tipo Int
                                                   sts' = map St indexes -- mis nuevos estados son los indices que consegui arriba asi que simplemente los mapeo con el constructor de estados para tener el tipo correcto
                                                   f' = map (\(st, t, st') -> (stateRename st, t, stateRename st')) f -- mi nueva funcion de transicion es la misma pero con los nombres de los estados cambiados por la funcion descripta anteriormente stateRename
                                                   stsa' = map stateRename stsa -- al igual que la funcion de transicion, mis estados de aceptacion son los mismos solo que los llamo con Ints usando stateRename
                                                   sti' = stateRename sti -- igual que lo anterior, mismo estado solo que lo renombro a Int con stateRename
                                            in D simb sts' (FunT f') stsa' sti' b

-- funcion que toma dos automatas no deterministas
-- y devuelve otro donde sus estados son de tipo Int (ya que necesitamos normalizarlos a un mismo tipo y evitar repetirlos)
-- En la concatenacion tenemos que agregar lambda transitions que vayan de los
-- estados de aceptacion del primer automata al inicial del segundo
-- además la concatenación tendra como conjunto de estados la union de los otros
-- lo mismo con el conjunto de simbolos
-- sus estados de aceptacion seran los estados de aceptacion del segundo
-- y su estado inicial sera el estado inicial del primero
-- todo esto obviamente renombrando los estados a tipo Int con un indice correspondiente para cada uno
concatAEFND :: (Eq a, Eq b) => AEFND a -> AEFND b -> AEFND Int
concatAEFND (ND simb sts (RelT r) stsa sti b) (ND simb' sts' (RelT r') stsa' sti' b') =
  let l = length sts
      l' = length sts'
      indexes = [0..(l + l') - 1] -- construyo una lista de 0 a ultimo indice necesario para representar mi cantidad de estados del primer automata + del segundo automata como ints
      stateRenameP st = St $ fromJust $ elemIndex st sts -- una funcion trivial que simplemente renombra un estado de tipo `a` a tipo Int para el primer automata
      stateRenameS st = St $ l + fromJust (elemIndex st sts') -- una funcion trivial que simplemente renombra un estado de tipo `a` a tipo Int para el segundo automata buscando su indice en la lista de estados original y luego sumandole un desplazamiento de la cantidad de estados del primer automata
      sts'' = map St indexes -- mis nuevos estados son los indices que consegui arriba asi que simplemente los mapeo con el constructor de estados para tener el tipo correcto
      stiS = stateRenameS sti' -- el estado inicial del segundo automata renombrado
      r'' = map (\(st, t, st') -> (stateRenameP st, t, stateRenameP st')) r -- la relacion del primer automata llevada a los renombres correspondientes de los estados
         ++ map (\(st, t, st') -> (stateRenameS st, t, stateRenameS st')) r' -- la relacion del segundo automata llevada a los renombres correspondientes de los estados
         ++ [(stateRenameP st, SimbND "", stiS) | st <- stsa] -- las lambda transitions desde los estados de aceptacion del primer automata al estado inicial del segundo
      stsa'' = map stateRenameS stsa' -- mis nuevos estados de aceptacion son los estados de aceptacion del segundo automata mapeados con sus nuevos nombres
      sti'' = stateRenameP sti -- mi nuevo estado inicial es el estado inicial del primer automata renombrado
  in ND (simb `union` simb') sts'' (RelT r'') stsa'' sti'' (b && b')


aefdToAEFND :: AEFD a -> AEFND a
aefdToAEFND (D simb sts (FunT f) stsa sti b) = let simb' = map (\(SimbD x) -> SimbND $ NE.toList x) simb
                                                   r = map (\(st, SimbD x, st') -> (st, SimbND $ NE.toList x, st')) f
                                               in ND simb' sts (RelT r) stsa sti b