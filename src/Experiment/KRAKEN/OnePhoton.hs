module Experiment.KRAKEN.OnePhoton where

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


kraken1ph :: QState()
kraken1ph = whenRunKraken1ph . join $ kraken1phForQNums
    <$>getReadOption "kappas0"<*>getReadOption "ns0"<*>getReadOption "kappas1"

kraken1phForQNums :: [QNum] -> [QNum] -> [QNum] -> QState()
kraken1phForQNums kappas0 ns0 kappas1 = whenRunKraken1ph $
    getDensityMatrix kappas0 ns0 kappas1>>=
    forM_ [ saveData "Rho1ph"
          , saveData "Purity1ph"      . purity
          , saveData "Concurrence1ph" . concurrence
          ] . flip ($)
    where saveData key val = whenM (getReadOption ("save"   ++key))
                                $ printQStateFile ("outFile"++key) val

whenRunKraken1ph :: QState() -> QState()
whenRunKraken1ph = whenM (getReadOption "runKRAKEN1ph")



getDensityMatrix :: [QNum] -> [QNum] -> [QNum] -> QState DensityMatrix
getDensityMatrix kappas0 ns0 kappas1 = fromStates<$>zipWithM
    (curry (flip (uncurry getPureState) kappas1)) kappas0 ns0

getPureState :: QNum -> QNum -> [QNum] -> QState Ket
getPureState kappa0 n0 kappas1 = energyKetToEkinGrid
    =<<getInterpolatedExcitedStateByEkin kappa0 n0 kappas1