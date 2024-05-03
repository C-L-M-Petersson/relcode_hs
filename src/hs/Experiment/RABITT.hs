module Experiment.RABITT
(   rabitt
,   rabittForQNums
) where

import           Control.Monad.Extra

import           Experiment.RABITT.Atomic
import           Experiment.RABITT.CC
import           Experiment.RABITT.Common
import           Experiment.RABITT.Wigner

import           Maths.QuantumNumbers

import           QState
import           QState.Configure

import Maths.HilbertSpace.Scalar
import QState.Output
import           QState.TwoPhoton

rabitt :: QState()
rabitt = (whenRunRABITT $ forGroundStates_ rabittForQNums)

rabittForQNums :: QNum -> QNum -> QState()
rabittForQNums kappa0 n0 = do
    es <- getSideBandEnergy kappa0 n0

    saveWigner <- getReadOption "saveWignerRABITT"
    saveAtomic <- getReadOption "saveAtomicRABITT"
    saveCC     <- getReadOption "saveCCRABITT"

    phaseW  <- if saveWigner||saveCC then getReadOption "kappas1"
                                            >>=calcWignerPhaseForQNums kappa0 n0
                                      else return []
    phaseAt <- if saveAtomic||saveCC then getReadOption "kappas2"
                                            >>=calcAtomicPhaseForQNums kappa0 n0
                                     else return []
    let phaseCC = calcCCPhaseFromPhases phaseW phaseAt

    when saveWigner $ savePhase "Wigner" es phaseW
    when saveAtomic $ savePhase "Atomic" es phaseAt
    when saveCC     $ savePhase "CC"     es phaseCC
