module PPrint (
  printGram,
  render
)
where

import           Common
import           Grammar
-- import           Text.PrettyPrint.HughesPJ
-- import           Prelude                 hiding ( (<>) )

import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, color, colorDull, Color (..), AnsiStyle )
import Prettyprinter
    ( annotate,
      defaultLayoutOptions,
      layoutSmart,
      sep,
      vsep,
      pipe,
      dquotes,
      comma,
      Doc,
      Pretty(pretty) )
import Data.List ((\\), sort)

--Colores
nonTerminalColor :: Doc AnsiStyle -> Doc AnsiStyle
nonTerminalColor = annotate (color Blue)
arrowColor :: Doc AnsiStyle -> Doc AnsiStyle
arrowColor = annotate (color Magenta <> italicized)
bnfColor :: Doc AnsiStyle -> Doc AnsiStyle
bnfColor = annotate (colorDull Cyan) -- <> bold)
terminalColor :: Doc AnsiStyle -> Doc AnsiStyle
terminalColor = annotate (color Green)

nt2doc :: NT -> Doc AnsiStyle
nt2doc Initial = nonTerminalColor (pretty "&")
nt2doc (NT nt) = nonTerminalColor (pretty nt)

t2doc :: T -> Doc AnsiStyle
t2doc t = dquotes $ terminalColor (pretty $ runT t)

printTs :: [T] -> Doc AnsiStyle
printTs [] = pretty ""
printTs [t] = t2doc t
printTs (t:ts) = t2doc t <> comma <> printTs ts

printSimb :: [Rule] -> Doc AnsiStyle
printSimb rs = let (ts, _) = tsNTsFromRules rs
               in pretty "Alfabeto: {" <> printTs (sort ts \\ [T ""]) <> pretty "}\n" -- le saco el lambda

printRightSideLeft :: RigthSide -> Doc AnsiStyle
printRightSideLeft (RT t) = t2doc t
printRightSideLeft (RTNT t nt) = sep $ nt2doc nt:[t2doc t]
printRightSideLeft RL = terminalColor $ pretty "\\"

printRightSideRight :: RigthSide -> Doc AnsiStyle
printRightSideRight (RT t) = t2doc t
printRightSideRight (RTNT t nt) = sep $ t2doc t:[nt2doc nt]
printRightSideRight RL = terminalColor $ pretty "\\"

printRightSidesLeft :: [RigthSide] -> [Doc AnsiStyle]
printRightSidesLeft [rs] = [printRightSideLeft rs]
printRightSidesLeft (rs:rss) = printRightSideLeft rs:(bnfColor pipe:printRightSidesLeft rss)
printRightSidesLeft [] = error "no deberia pasar"

printRightSidesRight :: [RigthSide] -> [Doc AnsiStyle]
printRightSidesRight [rs] = [printRightSideRight rs]
printRightSidesRight (rs:rss) = printRightSideRight rs:(bnfColor pipe:printRightSidesRight rss)
printRightSidesRight [] = error "no deberia pasar"

printRuleLeft :: Rule -> Doc AnsiStyle
printRuleLeft (Rule nt rss) = sep $ nt2doc nt:(arrowColor (pretty "->"):printRightSidesLeft rss)

printRuleRight :: Rule -> Doc AnsiStyle
printRuleRight (Rule nt rss) = sep $ nt2doc nt:(arrowColor (pretty "->"):printRightSidesRight rss)

printRulesLeft :: [Rule] -> Doc AnsiStyle
printRulesLeft rs = vsep $ map printRuleLeft rs

printRulesRight :: [Rule] -> Doc AnsiStyle
printRulesRight rs = vsep $ map printRuleRight rs

printGramLeft :: GramTerm -> Doc AnsiStyle
printGramLeft (Gram []) = pretty "La gramatica no presenta reglas de producción :("
printGramLeft (Gram rus) = vsep $ printSimb rus: pretty "Reglas:" : [printRulesLeft rus]

printGramRight :: GramTerm -> Doc AnsiStyle
printGramRight (Gram []) = pretty "La gramatica no presenta reglas de producción :("
printGramRight (Gram rus) = vsep $ printSimb rus: pretty "Reglas:" : [printRulesRight rus]


printGram :: Gram -> Doc AnsiStyle
printGram = either printGramLeft printGramRight

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions