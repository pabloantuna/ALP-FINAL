module PPrint 
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

-- |  Pretty printer de nombres (String)
ppName :: Name -> String
ppName = id

-- parensIf :: Bool -> Doc AnsiStyle -> Doc AnsiStyle
-- parensIf True  = parens
-- parensIf _ = id

-- printGramTerm :: GramTerm -> Doc AnsiStyle
-- printGramTerm t = pp 0 (filter (\v -> v `notElem` (fv t)) vars) t

printGram :: Gram -> Doc AnsiStyle
printGram g = undefined