module QState.Coherence.Internal
(   Coherence(All,None,Angular,Spin)
,   kappasCoherent
,   kappasByCoherence

,   coherent
,   coherent1ph
,   coherent2ph
) where

import           Data.Composition
import           Data.List                 (partition)

import           Maths.QuantumNumbers

import           QState.Configure.Internal


data Coherence =       All |       None |       Angular |       Spin
               | ForcedAll | ForcedNone | ForcedAngular | ForcedSpin
    deriving(Read,Show)

kappasCoherent :: Bool -> Coherence -> QNum -> QNum -> Bool
kappasCoherent final   All     _ _  = not final
kappasCoherent final   None    k k' = not final||(       k==k'       )
kappasCoherent final   Angular k k' = not final||(    -1-k==k'       )
kappasCoherent final   Spin    k k' = not final||(signum k==signum k')
kappasCoherent _ ForcedAll     _ _  = True
kappasCoherent _ ForcedNone    k k' =        k==k'
kappasCoherent _ ForcedAngular k k' =     -1-k==k'
kappasCoherent _ ForcedSpin    k k' = signum k==signum k'

kappasByCoherence :: Bool -> Coherence -> [QNum] -> [[QNum]]
kappasByCoherence = groupRec.:kappasCoherent
    where
        groupRec :: (QNum -> QNum -> Bool) -> [QNum] -> [[QNum]]
        groupRec cTest (k:ks) = let (cKs,icKs) = partition (cTest k) ks
                                 in (k:cKs) : groupRec cTest icKs
        groupRec _      _     = []


coherent :: Int -> CDict -> Coherence
coherent nPh = cDictReadOption ("coherent"++show nPh++"ph")

coherent1ph :: CDict -> Coherence
coherent1ph = coherent 1

coherent2ph :: CDict -> Coherence
coherent2ph = coherent 2

