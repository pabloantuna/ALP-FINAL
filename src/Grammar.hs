module Grammar where

import Common
import FiniteAutomata
import Data.List ( union )

-- funcion para obtener a partir de una lista del lado derecho de una regla de produccion
-- una tupla con dos listas
-- la primera es la lista de terminales
-- la segunda es la lista de no terminales
tsNTsFromRightSide :: [RigthSide] -> ([T], [NT])
tsNTsFromRightSide [] = ([], [])
tsNTsFromRightSide ((RT t) : xs) = let (ts', nts) = tsNTsFromRightSide xs

                                   in (t:ts', nts)
tsNTsFromRightSide ((RTNT t nt) : xs) = let (ts', nts') = tsNTsFromRightSide xs
                                            ts = if t `elem` ts' then ts' else t:ts'
                                            nts = if nt `elem` nts' then nts' else nt:nts'
                                        in (ts, nts)
tsNTsFromRightSide (RL : xs) = let (ts', nts) = tsNTsFromRightSide xs
                                   ts = if T "" `elem` ts' then ts' else T "":ts'
                               in (ts, nts)

tsNTsFromRule :: Rule -> ([T], [NT])
tsNTsFromRule (Rule nt rs@(_:_)) = let (ts, nts') = tsNTsFromRightSide rs
                                       nts = if nt `elem` nts' then nts' else nt:nts'
                                    in (ts, nts)
tsNTsFromRule (Rule _ []) = error "no tiene sentido que una regla no genere nada"

-- funcion para pasar de la lista de reglas a una lista
-- de terminales y una de no terminales
tsNTsFromRules :: [Rule] -> ([T], [NT])
tsNTsFromRules [] = ([], [])
tsNTsFromRules (x:xs) = let (ts', nts') = tsNTsFromRules xs
                            (tss, ntss) = tsNTsFromRule x
                            ts = union tss ts'
                            nts = union ntss nts'
                          in (ts, nts)


-- funcion que dada una regla y una lista de reglas
-- devuelve una tupla con una regla nueva
-- que resulta de la unificacion de todas las reglas de la lista
-- que son reglas que tienen como generador al mismo no terminal
-- i.e unifica las reglas de la lista con la regla que se toma como primer arg
unificarRules' :: Rule -> [Rule] -> (Rule, [Rule])
unificarRules' r [] = (r, [])
unificarRules' r@(Rule nt rs) (r2@(Rule nt' rs'):xs) | nt == nt' = let (nr, rest) = unificarRules' (Rule nt (rs `union` rs')) xs
                                                                in (nr, rest)
                                                     | otherwise = let (nr, rest) = unificarRules' r xs
                                                                in (nr, r2:rest)

-- funcion para pasar de la lista de reglas a una lista
-- de reglas unificadas es decir las gramaticas que se hayan escrito
-- como A->g1 A->g2 van a pasar a estar como si hubiese sido parseado
-- A -> g1 | g2
unificarRules :: [Rule] -> [Rule]
unificarRules [] = []
unificarRules (x:xs) = let (r, rest) = unificarRules' x xs
                       in r:unificarRules rest

-- pasa un no terminal a su string correspondiente
ntToString :: NT -> String
ntToString (NT s) = s
ntToString Initial = "&"

-- dado un no terminal y una lista de reglas
-- determina si dicho no terminal genera la cadena vacia
-- i.e si dicho no terminal pasado a un estado seria un estado de aceptacion
isEstAcep :: NT -> [Rule] -> Bool
isEstAcep _ [] = False
isEstAcep nt ((Rule nt' rs):xs) | nt == nt' = RL `elem` rs
                                | otherwise = isEstAcep nt xs

-- determina si dado un no terminal, un terminal, otro no terminal, y una lista de reglas
-- existe una relacion que consumiendo el terminal se pasa
-- desde el primer no terminal tomado como estado al segundo no terminal tomado como estado
-- i.e si existe una generacion de la forma NT -> tNT'
isRel :: NT -> T -> NT -> [Rule] -> Bool
isRel _ _ _ [] = False
isRel nt t nt' ((Rule nt'' rs):xs) | nt == nt'' = RTNT t nt' `elem` rs
                                   | otherwise = isRel nt t nt' xs

-- determina si dado un no terminal, un terminal y una lista de reglas
-- el no terminal puede generar ese terminal sin pasar a otra regla de produccion
-- i.e si existe NT -> t
acceptTerminal :: NT -> T -> [Rule] -> Bool
acceptTerminal _ _ [] = False
acceptTerminal nt t ((Rule nt' rs):xs) | nt == nt' = RT t `elem` rs
                                       | otherwise = acceptTerminal nt t xs

-- una funcion simple que setea la bandera a false
-- la voy a usar despues de hacer el reverse de el automata
-- que obtengo al tratar la gramatica izquierda como derecha
flagToLeft :: AEFND a -> AEFND a
flagToLeft (ND sns sts rt st's st _) = ND sns sts rt st's st False

-- funcion que pasa de un GramTerm correspondiente a una gramatica derecha
-- a un automata no determinista
-- cuyos estados son Maybe String, ya que voy a tener que crear
-- un estado nuevo Nothing para tener un estado de aceptación nuevo
-- al cual irán todas las relaciones que tenga que construir
-- cuando este en un no terminal que genera un terminal y no tiene
-- ningun otro no terminal, i.e NT -> t => del estado NT puedo llegar a Nothing (estado aceptacion) tomando t
gramTermDerToAEFND :: GramTerm -> AEFND (Maybe String)
gramTermDerToAEFND (Gram rus) = let rules = unificarRules rus -- unifico las reglas para que sea mas facil de trabajar
                                    (ts, nts) = tsNTsFromRules rules -- obtengo los terminales y no terminales
                                    simb = map (SimbND . runT) ts -- los simbolos (alfabeto) del automata son los terminales de la gramatica
                                    sts  = St Nothing : map (St . Just . ntToString) nts -- los estados seran el nuevo estado de aceptacion Nothing + todos los no terminales
                                    r    = RelT (([((St . Just . ntToString) st1, (SimbND . runT) t, (St . Just . ntToString) st1) | st1 <- nts, t <- ts, st2 <- nts, isRel st1 t st2 rules]) `union` -- las relaciones que permiten pasar de un estado a otro (la regla NT -> NT' => existe una relacion (NT, "", NT'))
                                                ([((St . Just . ntToString) st1, (SimbND . runT) t, St Nothing) | st1 <- nts, t <- ts, acceptTerminal st1 t rules])) -- los estados que aceptan la entrada de un terminal pero no van a ningun otro estado (NT -> t => consumiendo t voy a Nothing, i.e existe la relacion (NT, t, Nothing))
                                    stsa = St Nothing : [(St . Just . ntToString) nt | nt <- nts, isEstAcep nt rules] -- los estados de aceptacion son el nuevo estado Nothing + los estados que acepten la cadena vacia
                                    sti  = St (Just "&") -- el estado inicial es por convencion del programa el que corresponde al simbolo '&' (el simbolo NT inicial de la gramatica)
                                  in ND simb sts r stsa sti True -- finalmente construyo el automata

-- funcion que pasa un GramTerm correspondiente a una gramatica izquierda
-- a un automata no determinista, esto lo logro
-- pasandolo como si fuese una gramatica derehca y realizando el reverse del automata generado
-- porque eso encontre en todos los foros (TODO: ver si hay otra forma)
-- finalmente le marco el bool del AEFND en false para indicar que es izquierda para mas adelante
gramTermIzqToAEFND :: GramTerm -> AEFND (Maybe (Maybe String))
gramTermIzqToAEFND = flagToLeft . reverseAefnd . gramTermDerToAEFND

-- funcion que pasa de un Gram a un Either de AEFND de izq o der
-- ya que como el reverse del automata devuelve estados Maybe pero el pasar
-- una gram derecha a AEFND tambien devuelve Maybe entonces la gramatica izquierda
-- termina siendo pasada a automata de estados Maybe Maybe mientras que la derecha simplemente Maybe
-- (igual capaz esta funcion directamente vuela y termino usando las otras de forma directa depende como note que es mas comodo)
gramToAEFND :: Gram -> Either (AEFND (Maybe (Maybe String))) (AEFND (Maybe String))
gramToAEFND = either (Left . gramTermIzqToAEFND) (Right . gramTermDerToAEFND)

gramIzqToAEFD :: GramTerm -> AEFD Int
gramIzqToAEFD = undefined

gramDerToAEFD :: GramTerm -> AEFD Int
gramDerToAEFD = undefined

gramToAEFD :: Gram -> AEFD Int
gramToAEFD = either gramIzqToAEFD gramDerToAEFD


-- RG ["a", "b", ""] ["A", "B", "&"] [RRNT "&" "a" "A", RRNT "&" "b" "B", RRL "&", RRNT "A" "b" "B", RRNT "A" "a" "&", RRT "B" "b"]
-- Right (Gram [Rule Initial [RTNT (T \"a\") (NT \"A\"),RTNT (T \"b\") (NT \"B\"),RL],Rule (NT \"A\") [RTNT (T \"b\") (NT \"B\"),RTNT (T \"a\") Initial],Rule (NT \"B\") [RT (T \"b\")]])

-- >>> descomponerRules ([Rule Initial [RTNT (T "a") (NT "A"),RTNT (T "b") (NT "B"),RL],Rule (NT "A") [RTNT (T "b") (NT "B"),RTNT (T "a") Initial],Rule (NT "B") [RT (T "b")]])
-- ([T {runT = "a"},T {runT = "b"},T {runT = ""}],[Initial,NT "A",NT "B"],[])


-- >>> unificarRules ([Rule Initial [RTNT (T {runT = "a"}) (NT "A"),RTNT (T {runT = "b"}) (NT "B"),RL],Rule (NT "A") [RTNT (T {runT = "b"}) (NT "B"),RTNT (T {runT = "a"}) Initial],Rule (NT "B") [RT (T {runT = "b"})], Rule (NT "A") [RTNT (T {runT = "c"}) (NT "A"),RTNT (T {runT = "c"}) Initial]])
--                    [Rule Initial [RTNT (T {runT = "a"}) (NT "A"),RTNT (T {runT = "b"}) (NT "B"),RL],Rule (NT "A") [RTNT (T {runT = "b"}) (NT "B"),RTNT (T {runT = "a"}) Initial,RTNT (T {runT = "c"}) (NT "A"),RTNT (T {runT = "c"}) Initial],Rule (NT "B") [RT (T {runT = "b"})]]

-- >>> unificarRules' (Rule (NT "A") [RTNT (T {runT = "b"}) (NT "B"),RTNT (T {runT = "a"}) Initial]) ([Rule (NT "B") [RT (T {runT = "b"})],Rule (NT "A") [RTNT (T {runT = "b"}) (NT "B"),RTNT (T {runT = "a"}) Initial]])
-- (Rule (NT "A") [RTNT (T {runT = "b"}) (NT "B"),RTNT (T {runT = "a"}) Initial],[Rule (NT "B") [RT (T {runT = "b"})]])


-- >>> NT "A" == NT "A"
-- True
