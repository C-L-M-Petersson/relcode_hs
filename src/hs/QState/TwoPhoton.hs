module QState.TwoPhoton
(   getEnergyRPA
,   getEnergyFin

,   getIROmegas

,   getMElements2ph
) where

import           Data.Composition
import           Data.List                 (transpose)

import           Maths.HilbertSpace.Scalar
import           Maths.QuantumNumbers

import           QState
import           QState.Coherence
import           QState.Configure
import           QState.PertWave
import           QState.TwoPhoton.Internal
import           QState.TwoPhoton.RPAE


getEnergyRPA :: QNum -> QNum -> QState [Double]
getEnergyRPA = withCDictM.:energyRPA

getEnergyFin :: QNum -> QNum -> Int -> QState [Double]
getEnergyFin = withCDictM.:.energyFin


getIROmegas :: Int -> QState [Double]
getIROmegas = withCDictM . irOmegas

getMElement2phCoherent :: QNum -> QNum -> [QNum] -> [QNum] -> QNum -> Int
                                                    -> Bool -> QState [Scalar]
getMElement2phCoherent kappa0 n0 kappas1 kappas2 mJ eFinalIndex use2phRPAE =
    map sum . transpose<$>sequence [ timesXi kappa2 mJ=<<withCDictM (\cDict ->
        if use2phRPAE then zipWith (+)<$>m0 kappa2 cDict
                                      <*>dm kappa2 cDict
                      else m0 kappa2 cDict)
                | kappa2 <- kappas2 ]
        where
            m0 kappa2 cDict = mElement2phKappas1    kappa0 n0 kappas1 kappa2 mJ
                                                            eFinalIndex cDict
            dm kappa2 cDict = mElement2phCorrection kappa0 n0         kappa2 mJ
                                                            eFinalIndex cDict

getMElements2ph :: QNum -> QNum -> [QNum] -> QNum -> Int -> QState [[Scalar]]
getMElements2ph kappa0 n0 kappas2 mJ eFinalIndex = do
    kappas1s   <- onePhotonKappasGroupedByCoherence False
    kappas2s   <- groupTwoPhotonKappasByCoherence   True  kappas2
    use2phRPAE <- getReadOption "use2phRPAE"

    sequence [ getMElement2phCoherent kappa0 n0 kappas1 kappas2' mJ eFinalIndex
                                                                    use2phRPAE
                | kappas1 <- kappas1s, kappas2' <- kappas2s ]
