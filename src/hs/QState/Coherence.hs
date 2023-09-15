module QState.Coherence
(   groupOnePhotonKappasByCoherence
,   groupTwoPhotonKappasByCoherence
) where

import           Maths.QuantumNumbers

import           QState
import           QState.Coherence.Internal


groupOnePhotonKappasByCoherence :: [QNum] -> QState [[QNum]]
groupOnePhotonKappasByCoherence ks = withCDict
                                   $ (`kappasByCoherence`ks) . coherent1ph

groupTwoPhotonKappasByCoherence :: [QNum] -> QState [[QNum]]
groupTwoPhotonKappasByCoherence ks = withCDict
                                   $ (`kappasByCoherence`ks) . coherent2ph
