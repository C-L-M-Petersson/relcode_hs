module Experiment.KRAKEN.TwoPhoton where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Extra

import           Data.Composition
import           Data.List

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
                                            kappas1 kappas2 eFinalIndex
        >>=forM_ [ saveData printEnergyDistributionQStateFile "Rho2ph"
                 , saveData printQStateFile "Purity2ph"      . purity
                 , saveData printQStateFile "Concurrence2ph" . concurrence
                 ] . flip ($)
        >>getPureStateSumByOnePhotonEnergy kappas0 ns0
                                            kappas1 kappas2 eFinalIndex
            >>=saveData printEnergyDistributionQStateFile "Psi2ph"
    where saveData print key val = whenM (getReadOption ("save"++key))
                                 $ print ("outFile"++key) val

whenRunKraken2ph :: QState() -> QState()
whenRunKraken2ph = whenM (getReadOption "runKRAKEN2ph")



getReconstructedOnePhotonDensityMatrix ::  [QNum] -> [QNum] -> [QNum] -> [QNum]
                                                -> Int -> QState DensityMatrix
getReconstructedOnePhotonDensityMatrix =
                            (fromStates<$>).::.getPureStatesByOnePhotonEnergy

getPureStateSumByOnePhotonEnergy :: [QNum] -> [QNum] -> [QNum] -> [QNum] -> Int
                                                                -> QState Ket
getPureStateSumByOnePhotonEnergy = (sum<$>).::.getPureStatesByOnePhotonEnergy

getPureStatesByOnePhotonEnergy :: [QNum] -> [QNum] -> [QNum] -> [QNum] -> Int
                                                                -> QState [Ket]
getPureStatesByOnePhotonEnergy kappas0 ns0 kappas1 kappas2 eFinalIndex = do
    coherent1Ph <- getReadOption "coherent1Ph"
    coherent2Ph <- getReadOption "coherent2Ph"

    let kappas1s = if coherent1Ph then [kappas1] else singleton`map`kappas1
        kappas2s = if coherent2Ph then [kappas2] else singleton`map`kappas2

    sequence
        [ getPureStateByOnePhotonEnergy kappa0 n0 kappas1' kappas2' eFinalIndex
            | (kappa0,n0) <- zip kappas0 ns0
            ,  kappas1'   <- kappas1s
            ,  kappas2'   <- kappas2s
            ]

getPureStateByOnePhotonEnergy :: QNum -> QNum -> [QNum] -> [QNum] -> Int
                                                                -> QState Ket
getPureStateByOnePhotonEnergy kappa0 n0 kappas1 kappas2 eFinalIndex =
    join (interpolatedExcitedState<$>getOmegas kappa0 n0
                                  <*>getMElements [kappa0] [n0]
                                                   kappas1 kappas2 eFinalIndex)
        >>=((shiftBasis<$>getHFEnergy kappa0 n0)??)>>=energyKetToEkinGrid
