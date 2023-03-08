{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Grammar where

import Common
import FiniteAutomata
import Data.List ( nub, sort, delete, elemIndex )
import Data.Char ( chr )

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
                                   ts = if "" `elem` ts' then ts' else "":ts'
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
                            ts = nub $ tss ++ ts'
                            nts = nub $ ntss ++ nts'
                          in (ts, nts)


-- funcion que dada una regla y una lista de reglas
-- devuelve una tupla con una regla nueva
-- que resulta de la unificacion de todas las reglas de la lista
-- que son reglas que tienen como generador al mismo no terminal
-- i.e unifica las reglas de la lista con la regla que se toma como primer arg
unificarRules' :: Rule -> [Rule] -> (Rule, [Rule])
unificarRules' r [] = (r, [])
unificarRules' r@(Rule nt rs) (r2@(Rule nt' rs'):xs) | nt == nt' = let (nr, rest) = unificarRules' (Rule nt (nub $ rs ++ rs')) xs
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
gramTermDerToAEFND (Gram rus) = let rules = unificarRules rus
                                    (ts, nts) = tsNTsFromRules rules
                                    simb = ts
                                    sts  = Nothing : map (Just . ntToString) nts
                                    r    = (nub $ ([((Just . ntToString) st1, t, (Just . ntToString) st2) | st1 <- nts, t <- ts, st2 <- nts, isRel st1 t st2 rules])
                                               ++ ([((Just . ntToString) st1, t, Nothing) | st1 <- nts, t <- ts, acceptTerminal st1 t rules]))
                                    stsa = Nothing : [(Just . ntToString) nt | nt <- nts, isEstAcep nt rules]
                                    sti  = Just "&"
                                  in ND simb sts r stsa sti True

-- funcion que pasa un GramTerm correspondiente a una gramatica izquierda
-- a un automata no determinista, esto lo logro
-- pasandolo como si fuese una gramatica derehca y realizando el reverse del automata generado
-- finalmente le marco el bool del AEFND en false para indicar que es izquierda para mas adelante
gramTermIzqToAEFND :: GramTerm -> AEFND (Maybe (Maybe String))
gramTermIzqToAEFND = flagToLeft . reverseAEFND . gramTermDerToAEFND

gramIzqToAEFDG :: GramTerm -> AEFDG
gramIzqToAEFDG = aefdToAEFDG . aefndToAEFD . gramTermIzqToAEFND

gramDerToAEFDG :: GramTerm -> AEFDG
gramDerToAEFDG = aefdToAEFDG . aefndToAEFD . gramTermDerToAEFND

gramToAEFDG :: Gram -> AEFDG
gramToAEFDG = either gramIzqToAEFDG gramDerToAEFDG

aefdForPrinting :: Eq a => AEFD a -> AEFD String
aefdForPrinting (D simb sts f stsa sti b) =
  let stsp = delete sti sts
      renameNT i = chr (65 + (i `mod` 26)):show (i `div` 26)
      stateRename st = case elemIndex st stsp of
                              Nothing -> if st == sti then "&" else error "No tiene sentido encontrar un estado que no este en esta lista y no sea el inicial"
                              Just i -> renameNT i
      f' = map (\(st, t, st') -> (stateRename st, t, stateRename st')) f
      stsa' = map stateRename stsa
      sti' = "&"
      sts' = sti':map stateRename stsp
  in D simb sts' f' stsa' sti' b

funToRule :: (Eq a, Show a) => St a -> (St a, SimbD, St a) -> Rule
funToRule sti (s, t, s') | s == sti = Rule Initial [RTNT t (NT $ show s')]
                         | otherwise = Rule (NT $ show s) [RTNT t (if sti == s' then Initial else NT $ show s')]

funToRules :: (Eq a, Show a) => [(St a, SimbD, St a)] -> St a -> [Rule]
funToRules r sti = map (funToRule sti) r

staToFinishRule :: (Eq a, Show a) => St a -> St a -> Rule
staToFinishRule sti st | st == sti = Rule Initial [RL]
                       | otherwise = Rule (NT $ show st) [RL]

stsaToFinishRules :: (Eq a, Show a) => St a -> [St a] -> [Rule]
stsaToFinishRules sti = map (staToFinishRule sti)

aefdToGramDer :: (Ord a) => AEFD a -> GramShow
aefdToGramDer aefd = let (D simb _ f stsa sti _) = aefdForPrinting $ removeDeadStates $ minimizeAEFD aefd
                         rus = sort $ unificarRules $ funToRules f sti ++ stsaToFinishRules sti stsa
                     in Right $ GramShow simb $ if null rus then [] else last rus:init rus

aefdToGramIzq :: (Ord a) => AEFD a -> GramShow
aefdToGramIzq aefd = let (D simb _ f stsa sti _) = aefdForPrinting $ removeDeadStates $ minimizeAEFD $ reverseAEFD aefd
                         rus = sort $ unificarRules $ funToRules f sti ++ stsaToFinishRules sti stsa
                     in Left $ GramShow simb $ if null rus then [] else last rus:init rus

aefdToGram :: (Ord a) =>AEFD a -> GramShow
aefdToGram aefd@(D _ _ _ _ _ b) = if b then aefdToGramDer aefd else aefdToGramIzq aefd