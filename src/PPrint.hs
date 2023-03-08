module PPrint (
  printGram,
  render
)
where

import           Common

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
nt2doc (NT nt) = nonTerminalColor (pretty (tail $ init nt))

t2doc :: T -> Doc AnsiStyle
t2doc t = dquotes $ terminalColor (pretty t)

printTs :: [T] -> Doc AnsiStyle
printTs [] = pretty ""
printTs [t] = t2doc t
printTs (t:ts) = t2doc t <> comma <> printTs ts

printSimb :: [T] -> Doc AnsiStyle
printSimb ts = pretty "Alfabeto: {" <> printTs (sort ts \\ [""]) <> pretty "}\n" -- le saco el lambda

printRightSideLeft :: RigthSide -> Doc AnsiStyle
printRightSideLeft (RT t) = t2doc t
printRightSideLeft (RTNT t nt) = sep $ nt2doc nt:[t2doc t]
printRightSideLeft RL = terminalColor $ pretty "\\"

printRightSideRight :: RigthSide -> Doc AnsiStyle
printRightSideRight (RT t) = t2doc t
printRightSideRight (RTNT t nt) = sep $ t2doc t:[nt2doc nt]
printRightSideRight RL = terminalColor $ pretty "\\"

printRightSidesLeft :: [RigthSide] -> [Doc AnsiStyle]
printRightSidesLeft [rs] = [printRightSideLeft rs <> pretty ";"]
printRightSidesLeft (rs:rss) = printRightSideLeft rs:(bnfColor pipe:printRightSidesLeft rss)
printRightSidesLeft [] = error "no deberia pasar"

printRightSidesRight :: [RigthSide] -> [Doc AnsiStyle]
printRightSidesRight [rs] = [printRightSideRight rs <> pretty ";"]
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

printGramLeft :: GramShowTerm -> Doc AnsiStyle
printGramLeft (GramShow _ []) = pretty "La gramatica no presenta reglas de producción :("
printGramLeft (GramShow ts rus) = vsep $ printSimb ts: pretty "Reglas:" : [printRulesLeft rus]

printGramRight :: GramShowTerm -> Doc AnsiStyle
printGramRight (GramShow _ []) = pretty "La gramatica no presenta reglas de producción :("
printGramRight (GramShow ts rus) = vsep $ printSimb ts: pretty "Reglas:" : [printRulesRight rus]


printGram :: GramShow -> Doc AnsiStyle
printGram = either printGramLeft printGramRight

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions