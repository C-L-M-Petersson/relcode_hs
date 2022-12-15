module QState.OnePhoton where

import           Data.Composition
import           Data.List

import           Maths.HilbertSpace
import           Maths.Interpolate
import           Maths.QuantumNumbers

import           QState
import           QState.Energy
import           QState.HartreeFock
import           QState.OnePhoton.Internal


getOmegas :: QNum -> QNum -> QState [Double]
getOmegas = withCDictM.:omegas

getAmps   :: QNum -> QNum -> QNum -> QState [Double]
getAmps   = withCDictM.:.amps

getPhaseF :: QNum -> QNum -> QNum -> QState [Double]
getPhaseF = withCDictM.:.phaseF

getPhaseG :: QNum -> QNum -> QNum -> QState [Double]
getPhaseG = withCDictM.:.phaseG



getMatElem :: QNum -> QNum -> QNum -> QState [Scalar]
getMatElem = withCDictM.:.matElem

getMatElems :: [QNum] -> [QNum] -> [QNum] -> QState [Scalar]
getMatElems  []               []       _      = return $ repeat 0
getMatElems (kappa0:kappas0) (n0:ns0) kappas1 = do
    e  <- map sum . transpose<$>mapM (getMatElem kappa0 n0) kappas1
    e' <- getMatElems kappas0 ns0 kappas1
    return $ zipWith (+) e e'

getMatElemKet :: [QNum] -> [QNum] -> [QNum] -> QState Ket
getMatElemKet kappas0 ns0 kappas1 = let kappa0 = head kappas0
                                        n0     = head ns0
                                     in Ket<$>(Just<$>getOmegas kappa0 n0)
                                           <*>getMatElems kappas0 ns0 kappas1

getInterpolatedMatElemKet :: [QNum] -> [QNum] -> [QNum] -> QState Ket
getInterpolatedMatElemKet kappas0 ns0 kappas1 = interpolateEnergyKet=<<
                                            getMatElemKet kappas0 ns0 kappas1


getExcitedStateByOmega :: [QNum] -> [QNum] -> [QNum] -> QState Ket
getExcitedStateByOmega kappas0 ns0 kappas1 = (*)<$>getXUVKet
                                <*>getMatElemKet kappas0 ns0 kappas1

getExcitedStateByEkin :: QNum -> QNum -> [QNum] -> QState Ket
getExcitedStateByEkin kappa0 n0 kappas1 = shiftBasis<$>getHFEnergy kappa0 n0
                                <*>getExcitedStateByOmega [kappa0] [n0] kappas1


getInterpolatedExcitedStateByOmega :: [QNum] -> [QNum] -> [QNum] -> QState Ket
getInterpolatedExcitedStateByOmega kappas0 ns0 kappas1 = (*)
    <$>getInterpolatedXUVKet<*>getInterpolatedMatElemKet kappas0 ns0 kappas1

getInterpolatedExcitedStateByEkin :: QNum -> QNum -> [QNum] -> QState Ket
getInterpolatedExcitedStateByEkin kappa0 n0 kappas1 = shiftBasis
                    <$>getHFEnergy kappa0 n0
                    <*>getInterpolatedExcitedStateByOmega [kappa0] [n0] kappas1
