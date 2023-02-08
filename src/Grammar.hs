module Grammar where

import Common

gramIzqToAEFND :: GramTerm -> AEFD Int
gramIzqToAEFND = undefined

gramDerToAEFND :: GramTerm -> AEFD Int
gramDerToAEFND = undefined

gramToAEFND :: Gram -> AEFD Int
gramToAEFND = undefined

gramIzqToAEFD :: GramTerm -> AEFD Int
gramIzqToAEFD = undefined

gramDerToAEFD :: GramTerm -> AEFD Int
gramDerToAEFD = undefined

gramToAEFD :: Gram -> AEFD Int
gramToAEFD = either gramIzqToAEFD gramDerToAEFD


-- RG ["a", "b", ""] ["A", "B", "&"] [RRNT "&" "a" "A", RRNT "&" "b" "B", RRL "&", RRNT "A" "b" "B", RRNT "A" "a" "&", RRT "B" "b"]