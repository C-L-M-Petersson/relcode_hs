module QState.OnePhoton where

import           Data.Composition
import           Data.List

import           Maths.HilbertSpace
import           Maths.QuantumNumbers

import           QState
import           QState.Light
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


getExcitedState :: [QNum] -> [QNum] -> [QNum] -> QState Ket
getExcitedState kappas0 ns0 kappas1 = ((*)<$>getXUVKet)
                                   <*>(ket<$>getMatElems kappas0 ns0 kappas1)
