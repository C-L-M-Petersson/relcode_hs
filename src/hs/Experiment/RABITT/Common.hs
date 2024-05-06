module Experiment.RABITT.Common
(   whenRunRABITT

,   forGroundStates
,   forGroundStates_

,   getSideBandEnergy

,   savePhase

,   toPhaseOrDelay

,   ketAbsEmi
,   ketAbs
,   ketEmi
) where

import           Control.Lens
import           Control.Monad.Extra

import           Data.Composition

import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar
import           Maths.QuantumNumbers

import           QState
import           QState.Configure
import           QState.OnePhoton
import           QState.Output
import           QState.TwoPhoton
import           QState.Units

import QState.Units.Delay

whenRunRABITT :: QState() -> QState()
whenRunRABITT = whenM (getReadOption "runRABITT")


forGroundStates :: (QNum -> QNum -> QState a) -> QState [a]
forGroundStates f = do
    kappas0 <- getReadOption "kappas0"
    ns0     <- getReadOption "ns0"

    sequence  [ f kappa0 n0 | (kappa0,n0) <- kappas0`zip`ns0 ]

forGroundStates_ :: (QNum -> QNum -> QState a) -> QState()
forGroundStates_ = void . forGroundStates


getSideBandEnergy :: QNum -> QNum -> QState [Scalar]
getSideBandEnergy kappa0 n0 = do
    irStepFraction <- getReadOption "IRStepFractionRABITT"
    getEs kappa0 n0>>=
        mapM (toUnits . setUnit Energy . fromReal) . drop irStepFraction


savePhase :: (Show a,Show b) => String -> [a] -> [b] -> QState()
savePhase key es vs = whenM (getReadOption $ "save"++key++"RABITT")
                    . putStrQStateFile fp $ unlines (zipWith showPhase es vs)
    where
        showPhase e v = show e++" "++show v
        fp = "outFile"++key++"RABITT"


toPhaseOrDelay :: [Scalar] -> QState [Double]
toPhaseOrDelay ms = (map toReal<$>) . mapM toUnits
    =<<(zipWith setUnit<$>getDelayUnitTypesWithOmega??map scalarPhase ms)

getDelayUnitTypesWithOmega :: QState [UnitType]
getDelayUnitTypesWithOmega = getReadOption "eFinalIndexAbsRABITT">>=getIROmegas
            >>=(<$>getReadOption "IRStepFractionRABITT") . flip drop . map Delay


ketCut :: Ket -> Bool -> Int -> Ket
ketCut k end n = let uts = ketBasisUnit k
                     kBs = ketBasis     k
                     kEs = ketElems     k
                     l = length kEs
                     cut  | end       = take(l-n)
                          | otherwise = drop n
                  in ket uts (cut<$>kBs) (cut kEs)

ketAbsEmi :: Ket -> QState (Ket,Ket)
ketAbsEmi k = (,)<$>ketAbs k<*>ketEmi k

ketAbs :: Ket -> QState Ket
ketAbs k = ketCut k True  . (*2)<$>getReadOption "IRStepFractionRABITT"

ketEmi :: Ket -> QState Ket
ketEmi k = ketCut k False . (*2)<$>getReadOption "IRStepFractionRABITT"
