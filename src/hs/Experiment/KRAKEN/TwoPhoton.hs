module Experiment.KRAKEN.TwoPhoton
(   kraken2ph
,   kraken2phForQNums
,   whenRunKraken2ph

,   getReconstructedOnePhotonDensityMatrix
,   getPureStateSumByOnePhotonEnergy
,   getPureStatesByOnePhotonEnergy
,   getPureStateByOnePhotonEnergy
) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Extra

import           Data.Composition

import           Maths.HilbertSpace.Distribution
import           Maths.HilbertSpace.Evolving.WignerFunction
import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Operator.DensityMatrix
import           Maths.QuantumNumbers

import           QState
import           QState.Coherence
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
        >>=forM_ [ ifSaveData printQStateFileWithUnits "Rho"
                 , ifSaveData printQStateFile "Purity"      . purity
                 , ifSaveData printQStateFile "Concurrence" . concurrence
                 , ifSaveData printQStateFileWithUnits "WignerFunc"
                        <=<getWignerFunctionFromDensityMatrix
                 ] . flip ($)
        >>getPureStateSumByOnePhotonEnergy kappas0 ns0
                                            kappas1 kappas2 eFinalIndex
            >>=ifSaveData printQStateFileWithUnits "Psi"
    where ifSaveData save key val = whenM (getReadOption ("save"++key++"2ph"))
                                  $ save ("outFile"++key++"2ph") val

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
    groupedKappas1 <- groupOnePhotonKappasByCoherence kappas1
    groupedKappas2 <- groupTwoPhotonKappasByCoherence kappas2
    let mJs = let j = minimum $ map (maximum . map jFromKappa)
                                    [kappas0,kappas1,kappas2]
               in mValues j

    sequence [ getPureStateByOnePhotonEnergy kappa0 n0 kappas1' kappas2' [mJ]
                                                                     eFinalIndex
                | (kappa0,n0) <- zip kappas0 ns0
                ,  kappas1'   <- groupedKappas1
                ,  kappas2'   <- groupedKappas2
                ,  mJ         <- mJs
                ]

getPureStateByOnePhotonEnergy :: QNum -> QNum -> [QNum] -> [QNum] -> [QNum]
                                                            -> Int -> QState Ket
getPureStateByOnePhotonEnergy kappa0 n0 kappas1 kappas2 msJ eFinalIndex =
    join (interpolatedExcitedState<$>getOmegas kappa0 n0
                                  <*>getMElements [kappa0] [n0]
                                                   kappas1 kappas2
                                                   msJ eFinalIndex)
        >>=((shiftBasis<$>getHFEnergy kappa0 n0)??)>>=energyKetToEGrid
