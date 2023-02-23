module PPrint (
  printGram,
  render
)
where

import           Common
-- import           Text.PrettyPrint.HughesPJ
-- import           Prelude                 hiding ( (<>) )

import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, color, colorDull, Color (..), AnsiStyle )
import Prettyprinter
    ( (<+>),
      annotate,
      defaultLayoutOptions,
      layoutSmart,
      nest,
      sep,
      parens,
      pipe,
      dquotes,
      Doc,
      Pretty(pretty) )
import Data.List (delete, groupBy)
import Data.Function (on)

--Colores
nonTerminalColor :: Doc AnsiStyle -> Doc AnsiStyle
nonTerminalColor = annotate (color Blue)
arrowColor :: Doc AnsiStyle -> Doc AnsiStyle
arrowColor = annotate (color Magenta <> italicized)
bnfColor :: Doc AnsiStyle -> Doc AnsiStyle
bnfColor = annotate (colorDull Cyan) -- <> bold)
terminalColor :: Doc AnsiStyle -> Doc AnsiStyle
terminalColor = annotate (color Green)
nameColor :: Doc AnsiStyle -> Doc AnsiStyle
nameColor = id


-- | Pretty printer de nombres (Doc)
name2doc :: Name -> Doc AnsiStyle
name2doc n = nameColor (pretty n)

nt2doc :: NT -> Doc AnsiStyle
nt2doc Initial = nonTerminalColor (pretty "&")
nt2doc (NT nt) = nonTerminalColor (pretty nt)

t2doc :: T -> Doc AnsiStyle
t2doc t = dquotes $ terminalColor (pretty $ runT t)

-- |  Pretty printer de nombres (String)
ppName :: Name -> String
ppName = id

-- parensIf :: Bool -> Doc AnsiStyle -> Doc AnsiStyle
-- parensIf True  = parens
-- parensIf _ = id

-- printGramTerm :: GramTerm -> Doc AnsiStyle
-- printGramTerm t = pp 0 (filter (\v -> v `notElem` (fv t)) vars) t

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
printRulesLeft rs = sep $ map printRuleLeft rs

printRulesRight :: [Rule] -> Doc AnsiStyle
printRulesRight rs = sep $ map printRuleRight rs

printGramLeft :: GramTerm -> Doc AnsiStyle
printGramLeft (Gram rus) = printRulesLeft rus

printGramRight :: GramTerm -> Doc AnsiStyle
printGramRight (Gram rus) = printRulesRight rus


printGram :: Gram -> Doc AnsiStyle
printGram = either printGramLeft printGramRight

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions