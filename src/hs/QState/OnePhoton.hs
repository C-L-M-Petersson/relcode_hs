module QState.OnePhoton
(   getOmegas
,   getEkins
,   getAmps
,   getPhaseF
,   getPhaseG

,   getMatElem
,   getMatElems

,   getInterpolatedExcitedStateByOmega
,   getInterpolatedExcitedStateByEkin
) where

import           Control.Monad

import           Data.Composition
import           Data.List                 (transpose)

import           Maths.HilbertSpace
import           Maths.QuantumNumbers

import           QState
import           QState.Energy
import           QState.HartreeFock
import           QState.OnePhoton.Internal


getOmegas :: QNum -> QNum -> QState [Double]
getOmegas = withCDictM.:omegas

getEkins :: QNum -> QNum -> QState [Double]
getEkins kappa0 n0 = map . (+)<$>getHFEnergy kappa0 n0<*>getOmegas kappa0 n0

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
getMatElems (kappa0:kappas0) (n0:ns0) kappas1 = zipWith (+)
        <$>(map sum . transpose<$>mapM (getMatElem kappa0 n0) kappas1)
        <*>getMatElems kappas0 ns0 kappas1
getMatElems _                 _        _      = error
    "inconsistent number of kappa- and n ground state quantum numbers provided"


getInterpolatedExcitedStateByOmega :: [QNum] -> [QNum] -> [QNum] -> QState Ket
getInterpolatedExcitedStateByOmega kappas0 ns0 kappas1 = join $
    interpolatedExcitedState<$>getOmegas (head kappas0) (head ns0)
                            <*>getMatElems kappas0 ns0 kappas1

getInterpolatedExcitedStateByEkin :: QNum -> QNum -> [QNum] -> QState Ket
getInterpolatedExcitedStateByEkin kappa0 n0 kappas1 = shiftBasis
                    <$>getHFEnergy kappa0 n0
                    <*>getInterpolatedExcitedStateByOmega [kappa0] [n0] kappas1
