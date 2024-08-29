module Experiment.KRAKEN.TwoPhoton
(   kraken2ph
) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Extra

import           Maths.HilbertSpace.Distribution
import           Maths.HilbertSpace.Evolving.WignerFunction
import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Operator.DensityMatrix
import           Maths.HilbertSpace.Scalar
import           Maths.QuantumNumbers

import           QState
import           QState.Configure
import           QState.Energy
import           QState.HartreeFock
import           QState.OnePhoton
import           QState.Output
import           QState.TwoPhoton


kraken2ph :: QState()
kraken2ph = whenRunKraken2ph $
    (join $ getReconstructedOnePhotonDensityMatrix<$>getReadOption "kappas0"
                                                  <*>getReadOption "ns0")
        >>=forM_ [ ifSaveData printQStateFileWithUnits "Rho"
                 , ifSaveData printQStateFile "Purity"      . purity
                 , ifSaveData printQStateFile "Concurrence" . concurrence
                 , ifSaveData printQStateFileWithUnits "WignerFunc"
                        <=<getWignerFunctionFromDensityMatrix
                 ] . flip ($)
        >>getPureStateSumByOnePhotonEnergy
            >>=ifSaveData printQStateFileWithUnits "Psi"
    where ifSaveData save key val = whenM (getReadOption ("save"++key++"2ph"))
                                  $ save ("outFile"++key++"2ph") val

whenRunKraken2ph :: QState() -> QState()
whenRunKraken2ph = whenM (getReadOption "runKRAKEN2ph")

getPureStateSumByOnePhotonEnergy :: QState Ket
getPureStateSumByOnePhotonEnergy = do
    kappas0 <- getReadOption "kappas0"
    ns0     <- getReadOption "ns0"

    sum . concat
            <$>zipWithM getPureStatesByOnePhotonEnergyGroundState kappas0 ns0

getReconstructedOnePhotonDensityMatrix :: [QNum] -> [QNum]
                                                        -> QState DensityMatrix
getReconstructedOnePhotonDensityMatrix kappas0 ns0 =
    sum<$>zipWithM getDensityMatrixGroundState kappas0 ns0

getDensityMatrixGroundState :: QNum -> QNum -> QState DensityMatrix
getDensityMatrixGroundState kappa0 n0 = fromStates<$>
        getPureStatesByOnePhotonEnergyGroundState kappa0 n0

getPureStatesByOnePhotonEnergyGroundState :: QNum -> QNum -> QState [Ket]
getPureStatesByOnePhotonEnergyGroundState kappa0 n0 =
    getMatrixElementGroundState kappa0 n0
                    >>=mapM (groundStateByOnePhotonEnergyGroundState kappa0 n0)

groundStateByOnePhotonEnergyGroundState :: QNum -> QNum -> [Scalar]
                                                                -> QState Ket
groundStateByOnePhotonEnergyGroundState kappa0 n0 mE =
    (getOmegas kappa0 n0>>=(`interpolatedExcitedState`mE))
        >>=((shiftBasis<$>getHFEnergy kappa0 n0)??)>>=energyKetToEGrid

getMatrixElementGroundState :: QNum -> QNum -> QState [[Scalar]]
getMatrixElementGroundState kappa0 n0 = do
    kappas1     <- getReadOption "kappas1"
    kappas2     <- getReadOption "kappas2"
    eFinalIndex <- getReadOption "eFinalIndexKRAKEN"

    concat<$>sequence [ getMElements2ph kappa0 n0 kappas2 mJ eFinalIndex
                        | mJ <- mValuesKappas (kappa0:kappas1++kappas2) ]
