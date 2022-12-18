module Experiment.KRAKEN.TwoPhoton where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Extra

import           Maths.HilbertSpace.DensityMatrix
import           Maths.HilbertSpace.Distribution
import           Maths.HilbertSpace.Ket
import           Maths.QuantumNumbers

import           QState
import           QState.Configure
import           QState.Energy
import           QState.HartreeFock
import           QState.OnePhoton
import           QState.Output
import           QState.TwoPhoton


kraken2ph :: QState()
kraken2ph = whenRunKraken2ph . join $ kraken2phForQNums
    <$>getReadOption "kappas0"<*>getReadOption "ns0"
    <*>getReadOption "kappas1"<*>getReadOption "kappas2"
    <*>getReadOption "eFinalIndex"

kraken2phForQNums :: [QNum] -> [QNum] -> [QNum] -> [QNum] -> Int -> QState()
kraken2phForQNums kappas0 ns0 kappas1 kappas2 eFinalIndex = whenRunKraken2ph $
    getReconstructedOnePhotonDensityMatrix kappas0 ns0
                                            kappas1 kappas2 eFinalIndex>>=
    forM_ [ saveData "Rho2ph"
          , saveData "Purity2ph"      . purity
          , saveData "Concurrence2ph" . concurrence
          ] . flip ($)
    where saveData key val = whenM (getReadOption ("save"   ++key))
                                $ printQStateFile ("outFile"++key) val

whenRunKraken2ph :: QState() -> QState()
whenRunKraken2ph = whenM (getReadOption "runKRAKEN2ph")



getReconstructedOnePhotonDensityMatrix ::  [QNum] -> [QNum] -> [QNum] -> [QNum]
                                                -> Int -> QState DensityMatrix
getReconstructedOnePhotonDensityMatrix kappas0 ns0 kappas1 kappas2 eFinalIndex =
        fromStates<$>zipWithM getPureState kappas0 ns0
    where getPureState kappa0 n0 = getPureStateByOnePhotonEnergy kappa0 n0
                                                    kappas1 kappas2 eFinalIndex

getPureStateByOnePhotonEnergy :: QNum -> QNum -> [QNum] -> [QNum] -> Int
                                                                -> QState Ket
getPureStateByOnePhotonEnergy kappa0 n0 kappas1 kappas2 eFinalIndex =
    join (interpolatedExcitedState<$>getOmegas kappa0 n0
                                  <*>getMElements [kappa0] [n0]
                                                   kappas1 kappas2 eFinalIndex)
        >>=((shiftBasis<$>getHFEnergy kappa0 n0)??)>>=energyKetToEkinGrid
