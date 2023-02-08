module Grammar where

import Common

grmRIzqToGrm :: GrmRTerm -> LGrm
grmRIzqToGrm GrmInitial = _wv
grmRIzqToGrm (GrmNT nt) = _ww
grmRIzqToGrm (GrmT t) = _wx
grmRIzqToGrm GLambda = _wy
grmRIzqToGrm (GOr g g') = _wz
grmRIzqToGrm (GrmLNTT nt t) = _wA
grmRIzqToGrm (GrmLIT t) = _wB
grmRIzqToGrm (LOr g g') = _wC
grmRIzqToGrm (GenRule g g') = _wG
grmRIzqToGrm (LeftRule g g') = _wH
grmRIzqToGrm (G g g') = 
  let LG t nt r = grmRIzqToGrm g
      LG t' nt' r' = grmRIzqToGrm g'
  in LG (t ++ t') (nt ++ nt') (r ++ r')
grmRIzqToGrm (L g g') =
  let LG t nt r = grmRIzqToGrm g
      LG t' nt' r' = grmRIzqToGrm g'
  in LG (t ++ t') (nt ++ nt') (r ++ r')
grmRIzqToGrm _ = error "No deberia caer en este caso"

grmRDerToGrm :: GrmRTerm -> RGrm
grmRDerToGrm (G g g') = 
  let RG t nt r = grmRDerToGrm g
      RG t' nt' r' = grmRDerToGrm g'
  in RG (t ++ t') (nt ++ nt') (r ++ r')
grmRDerToGrm (R g g') = 
  let RG t nt r = grmRDerToGrm g
      RG t' nt' r' = grmRDerToGrm g'
  in RG (t ++ t') (nt ++ nt') (r ++ r')
grmRDerToGrm g = let rules = grmRDerToGrm' g
                  in RG [] [] rules

grmRDerToGrm' :: GrmRTerm -> [RulesR]
grmRDerToGrm' GrmInitial = _wM
grmRDerToGrm' (GrmNT s) = _wN
grmRDerToGrm' (GrmT s) = _wO
grmRDerToGrm' GLambda = _wP
grmRDerToGrm' (GOr grt grt') = _wQ
grmRDerToGrm' (GrmRTNT s str) = _wU
grmRDerToGrm' (GrmRTI s) = _wV
grmRDerToGrm' (ROr grt grt') = _wW

grmRDerToGrm' (GenRule (GrmNT nt) (GOr g gs)) = _wX
grmRDerToGrm' (GenRule GrmInitial (GOr g gs)) = _wX

grmRDerToGrm' (GenRule (GrmNT nt) (GrmT t)) = [RRT (NT nt) (T t)]
grmRDerToGrm' (GenRule (GrmNT nt) GLambda) = [RRL (NT nt)]
grmRDerToGrm' (GenRule GrmInitial (GrmT t)) = [RRT (NT "&") (T t)]
grmRDerToGrm' (GenRule GrmInitial GLambda) = [RRL (NT "&")]

grmRDerToGrm' (RightRule (GrmNT nt) (ROr (GrmRTNT t nt') rs)) = RRNT (NT nt) (T t) (NT nt'):grmRDerToGrm' rs
grmRDerToGrm' (RightRule (GrmNT nt) (ROr (GrmRTI t) rs)) = RRNT (NT nt) (T t) (NT "&"):grmRDerToGrm' rs
grmRDerToGrm' (RightRule (GrmNT nt) (ROr (GrmT t) rs)) = RRT (NT nt) (T t):grmRDerToGrm' rs
grmRDerToGrm' (RightRule (GrmNT nt) (ROr GLambda rs)) = RRL (NT nt):grmRDerToGrm' rs
grmRDerToGrm' (RightRule GrmInitial (ROr (GrmRTNT t nt) rs)) = ():grmRDerToGrm'
grmRDerToGrm' (RightRule GrmInitial (ROr (GrmRTI t) rs)) = ():grmRDerToGrm'
grmRDerToGrm' (RightRule GrmInitial (ROr (GrmT t) rs)) = ():grmRDerToGrm'
grmRDerToGrm' (RightRule GrmInitial (ROr GLambda rs)) = ():grmRDerToGrm'

grmRDerToGrm' (RightRule (GrmNT nt) (GrmRTNT t nt')) = [RRNT (NT nt) (T t) (NT nt')]
grmRDerToGrm' (RightRule (GrmNT nt) (GrmRTI t)) = [RRT (NT nt) (T t)]
grmRDerToGrm' (RightRule GrmInitial (GrmRTNT t nt)) = [RRNT (NT "&") (T t) (NT nt)]
grmRDerToGrm' (RightRule GrmInitial (GrmRTI t)) = [RRT (NT "&") (T t)]
grmRDerToGrm' _ = error "No deberia caer en este caso"

-- grmRTermRuleToRuleR
grmRToGrm :: GrmR -> Grm
grmRToGrm (Left g) = Left $ grmRIzqToGrm g
grmRToGrm (Right g) = Right $ grmRDerToGrm g

grmRIzqToAEFND :: GrmRTerm -> AEFD Int
grmRIzqToAEFND = undefined

grmRDerToAEFND :: GrmRTerm -> AEFD Int
grmRDerToAEFND = undefined

grmRToAEFND :: GrmR -> AEFD Int
grmRToAEFND = undefined

grmRIzqToAEFD :: GrmRTerm -> AEFD Int
grmRIzqToAEFD = undefined

grmRDerToAEFD :: GrmRTerm -> AEFD Int
grmRDerToAEFD = undefined

grmRToAEFD :: GrmR -> AEFD Int
grmRToAEFD = either grmRIzqToAEFD grmRDerToAEFD

-- Right (R (RightRule GrmInitial (ROr (GrmRTNT \"a\" \"A\") (ROr (GrmRTNT \"b\" \"B\") GLambda))) (R (RightRule (GrmNT \"A\") (ROr (GrmRTNT \"b\" \"B\") (GrmRTI \"a\"))) (GenRule (GrmNT \"B\") (GrmT \"b\"))))
-- RG ["a", "b", ""] ["A", "B", "&"] [RRNT "&" "a" "A", RRNT "&" "b" "B", RRL "&", RRNT "A" "b" "B", RRNT "A" "a" "&", RRT "B" "b"]

-- >>> grmRToGrm (Right (R (RightRule GrmInitial (ROr (GrmRTNT \"a\" \"A\") (ROr (GrmRTNT \"b\" \"B\") GLambda))) (R (RightRule (GrmNT \"A\") (ROr (GrmRTNT \"b\" \"B\") (GrmRTI \"a\"))) (GenRule (GrmNT \"B\") (GrmT \"b\")))))