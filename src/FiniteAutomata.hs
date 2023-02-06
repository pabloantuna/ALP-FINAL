module FiniteAutomata where
import Common

unionAefd = undefined

intersecAefd = undefined

diffAefd = undefined

concatAefd = undefined

complementAefd = undefined

reverseAefd = undefined

sideAefd :: AEFDG -> AEFDG
sideAefd (AEFD (g, b)) = AEFD (g, not b)
