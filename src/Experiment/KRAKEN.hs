module Experiment.KRAKEN where

import           Control.Monad
import           Control.Monad.Extra

import           Maths.HilbertSpace.DensityMatrix
import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Operator
import           Maths.QuantumNumbers

import           QState
import           QState.Configure
import           QState.Energy
import           QState.OnePhoton
import           QState.Output



kraken1ph :: [QNum] -> [QNum] -> [QNum] -> QState ()
kraken1ph kappas0 ns0 kappas1 = do
    rho <- getDensityMatrix kappas0 ns0 kappas1

    saveData "Rho1ph"                       rho
    saveData "Purity1ph"      $ purity      rho
    saveData "Concurrence1ph" $ concurrence rho
    where saveData key val = whenM (getReadOption ("save"   ++key))
                                $ printQStateFile ("outFile"++key) val

getDensityMatrix :: [QNum] -> [QNum] -> [QNum] -> QState Operator
getDensityMatrix kappas0 ns0 kappas1 = fromStates<$>zipWithM
    (curry (flip (uncurry getPureState) kappas1)) kappas0 ns0

getPureState :: QNum -> QNum -> [QNum] -> QState Ket
getPureState kappa0 n0 kappas1 = energyKetToEkinGrid
    =<<getInterpolatedExcitedStateByEkin kappa0 n0 kappas1

