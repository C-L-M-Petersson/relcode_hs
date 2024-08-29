module QState.Coherence
(   groupOnePhotonKappasByCoherence
,   groupTwoPhotonKappasByCoherence

,   onePhotonKappasGroupedByCoherence
,   twoPhotonKappasGroupedByCoherence
) where

import           Maths.QuantumNumbers

import           QState
import           QState.Coherence.Internal
import           QState.Configure


groupOnePhotonKappasByCoherence :: Bool -> [QNum] -> QState [[QNum]]
groupOnePhotonKappasByCoherence final ks = withCDict
    $ flip (kappasByCoherence final) ks . coherent1ph

groupTwoPhotonKappasByCoherence :: Bool -> [QNum] -> QState [[QNum]]
groupTwoPhotonKappasByCoherence final ks = withCDict
    $ flip (kappasByCoherence final) ks . coherent2ph


onePhotonKappasGroupedByCoherence :: Bool -> QState [[QNum]]
onePhotonKappasGroupedByCoherence final = getReadOption "kappas1"
                    >>=groupOnePhotonKappasByCoherence final

twoPhotonKappasGroupedByCoherence :: Bool -> QState [[QNum]]
twoPhotonKappasGroupedByCoherence final = getReadOption "kappas2"
                    >>=groupTwoPhotonKappasByCoherence final
