module Experiment.KRAKEN.OnePhoton where

import           Control.Monad
import           Control.Monad.Extra

import           Data.Composition
import           Data.List

import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Operator.DensityMatrix
import           Maths.HilbertSpace.Evolving.WignerFunction
import           Maths.QuantumNumbers

import           QState
import           QState.Coherence
import           QState.Configure
import           QState.Energy
import           QState.OnePhoton
import           QState.Output


kraken1ph :: QState()
kraken1ph = whenRunKraken1ph . join $ kraken1phForQNums
    <$>getReadOption "kappas0"<*>getReadOption "ns0"<*>getReadOption "kappas1"

kraken1phForQNums :: [QNum] -> [QNum] -> [QNum] -> QState()
kraken1phForQNums kappas0 ns0 kappas1 = whenRunKraken1ph $
    getDensityMatrix kappas0 ns0 kappas1
        >>=forM_ [ ifSaveData printQStateFileWithUnits "Rho"
                 , ifSaveData printQStateFile "Purity"      . purity
                 , ifSaveData printQStateFile "Concurrence" . concurrence
                 , ifSaveData printQStateFileWithUnits "WignerFunc"
                        <=<getWignerFunctionFromDensityMatrix
                 ] . flip ($)
        >>getPureStateSum kappas0 ns0 kappas1
                >>=ifSaveData printQStateFileWithUnits "Psi"
    where ifSaveData print key val = whenM (getReadOption ("save"++key++"1ph"))
                                   $ print ("outFile"++key++"1ph") val

whenRunKraken1ph :: QState() -> QState()
whenRunKraken1ph = whenM (getReadOption "runKRAKEN1ph")



getDensityMatrix :: [QNum] -> [QNum] -> [QNum] -> QState DensityMatrix
getDensityMatrix = (fromStates<$>) .:. getPureStates

getPureStateSum :: [QNum] -> [QNum] -> [QNum] -> QState Ket
getPureStateSum = (sum<$>) .:. getPureStates

getPureStates :: [QNum] -> [QNum] -> [QNum] -> QState [Ket]
getPureStates kappas0 ns0 kappas1 = do
    groupedKappas1 <- groupOnePhotonKappasByCoherence kappas1

    sequence
        [ getPureState kappa0 n0 kappas1'
            | (kappa0,n0) <- zip kappas0 ns0
            ,  kappas1'   <- groupedKappas1
            ]

getPureState :: QNum -> QNum -> [QNum] -> QState Ket
getPureState kappa0 n0 kappas1 = energyKetToEkinGrid
    =<<getInterpolatedExcitedStateByEkin kappa0 n0 kappas1
