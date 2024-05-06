module QState.OnePhoton
(   getEs
,   getOmegas
,   getEkin

,   getAmp
,   getPCur
,   getPhaseF
,   getPhaseG

,   getWaveNumbers

,   getMatElem
,   getMatElems

,   getExcitedState

,   getInterpolatedExcitedStateByOmega
,   getInterpolatedExcitedStateByEkin
) where

import           Control.Lens
import           Control.Monad

import           Data.Composition
import           Data.List                 (transpose)

import           Maths.HilbertSpace
import           Maths.QuantumNumbers

import           QState
import           QState.Energy
import           QState.HartreeFock
import           QState.OnePhoton.Internal
import           QState.Units.Internal
import           QState.PertWave


getEs :: QNum -> QNum -> QState [Double]
getEs kappa0 n0 = join $ selectOutputEGrid<$>getOmegas kappa0 n0
                                          <*>getEkin   kappa0 n0

getOmegas :: QNum -> QNum -> QState [Double]
getOmegas = withCDictM.:omegas

getEkin :: QNum -> QNum -> QState [Double]
getEkin = withCDictM.:eKins


getAmp   :: QNum -> QNum -> QNum -> QState [Double]
getAmp   = withCDictM.:.amps

getPCur   :: QNum -> QNum -> QNum -> QState [Double]
getPCur   = withCDictM.:.pCurs

getPhaseF :: QNum -> QNum -> QNum -> QState [Double]
getPhaseF = withCDictM.:.phaseF

getPhaseG :: QNum -> QNum -> QNum -> QState [Double]
getPhaseG = withCDictM.:.phaseG


getWaveNumbers :: QNum -> QNum -> QState [Double]
getWaveNumbers kappa0 n0 = getEkin kappa0 n0>>=waveNumberEs


getMatElem :: QNum -> QNum -> QNum -> QState [Scalar]
getMatElem = withCDictM.:.matElem

getMatElems :: [QNum] -> [QNum] -> [QNum] -> QState [Scalar]
getMatElems  []               []       _      = return $ repeat 0
getMatElems (kappa0:kappas0) (n0:ns0) kappas1 = zipWith (+)
        <$>(map sum . transpose<$>mapM (getMatElem kappa0 n0) kappas1)
        <*>getMatElems kappas0 ns0 kappas1
getMatElems _                 _        _      = error
    "inconsistent number of kappa- and n ground state quantum numbers provided"


getExcitedState :: QNum -> QNum -> [Scalar] -> QState Ket
getExcitedState kappa0 n0 = (ket (Just Energy) . Just<$>getEs kappa0 n0??)

getInterpolatedExcitedStateByOmega :: [QNum] -> [QNum] -> [QNum] -> QState Ket
getInterpolatedExcitedStateByOmega kappas0 ns0 kappas1 = join $
    interpolatedExcitedState<$>getOmegas (head kappas0) (head ns0)
                            <*>getMatElems kappas0 ns0 kappas1

getInterpolatedExcitedStateByEkin :: QNum -> QNum -> [QNum] -> QState Ket
getInterpolatedExcitedStateByEkin kappa0 n0 kappas1 = shiftBasis
                    <$>getHFEnergy kappa0 n0
                    <*>getInterpolatedExcitedStateByOmega [kappa0] [n0] kappas1
