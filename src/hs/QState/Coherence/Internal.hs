module QState.Coherence.Internal
(   Coherence(All,None,Angular,Spin)
,   addKappasCoherently

,   coherent
,   coherent1ph
,   coherent2ph

,   kappasByCoherence
) where

import           Data.List                 (partition)

import           Maths.QuantumNumbers

import           QState.Configure.Internal


data Coherence = All | None | Angular | Spin deriving(Read,Show)

addKappasCoherently :: Coherence -> QNum -> QNum -> Bool
addKappasCoherently All     _ _  = True
addKappasCoherently None    k k' =        k==k'
addKappasCoherently Angular k k' =     -1-k==k'
addKappasCoherently Spin    k k' = signum k==signum k'


coherent :: Int -> CDict -> Coherence
coherent nPh = cDictReadOption ("coherent"++show nPh++"ph")

coherent1ph :: CDict -> Coherence
coherent1ph = coherent 1

coherent2ph :: CDict -> Coherence
coherent2ph = coherent 2


kappasByCoherence :: Coherence -> [QNum] -> [[QNum]]
kappasByCoherence c = groupRec (addKappasCoherently c)
    where
        groupRec :: (QNum -> QNum -> Bool) -> [QNum] -> [[QNum]]
        groupRec cTest (k:ks) = let (cKs,icKs) = partition (cTest k) ks
                                 in (k:cKs) : groupRec cTest icKs
        groupRec _      _     = []
