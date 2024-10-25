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


getMatElem :: QNum -> QNum -> QNum -> QNum -> QState [Scalar]
getMatElem kappa0 n0 kappa1 mJ = withCDictM (matElem kappa0 n0 kappa1 mJ)
                                                    >>=timesXi kappa1 mJ

getMatElems :: [QNum] -> [QNum] -> [QNum] -> QNum -> QState [Scalar]
getMatElems  []               []      _       _  = return $ repeat 0
getMatElems (kappa0:kappas0) (n0:ns0) kappas1 mJ = zipWith (+)
        <$>(map sum . transpose<$>mapM (getMatElem kappa0 n0 mJ) kappas1)
        <*>getMatElems kappas0 ns0 kappas1 mJ
getMatElems _                 _        _      _ = error
    "inconsistent number of kappa- and n ground state quantum numbers provided"


getExcitedState :: QNum -> QNum -> [Scalar] -> QState Ket
getExcitedState kappa0 n0 = (ket (Just Energy) . Just<$>getEs kappa0 n0??)

getInterpolatedExcitedStateByOmega :: [QNum] -> [QNum] -> [QNum] -> QNum
                                                                 -> QState Ket
getInterpolatedExcitedStateByOmega kappas0 ns0 kappas1 mJ = join $
    interpolatedExcitedState<$>getOmegas (head kappas0) (head ns0)
                            <*>getMatElems kappas0 ns0 kappas1 mJ

getInterpolatedExcitedStateByEkin :: QNum -> QNum -> [QNum] -> QNum
                                                                -> QState Ket
getInterpolatedExcitedStateByEkin kappa0 n0 kappas1 mJ = shiftBasis
                <$>getHFEnergy kappa0 n0
                <*>getInterpolatedExcitedStateByOmega [kappa0] [n0] kappas1 mJ
