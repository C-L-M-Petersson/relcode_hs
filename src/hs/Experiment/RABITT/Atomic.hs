module Experiment.RABITT.Atomic
(   calcAndSaveAtomicPhase
,   calcAndSaveAtomicPhaseForQNums

,   calcAtomicPhases
,   calcAtomicPhaseForQNums
) where

import           Control.Lens ((??))
import           Control.Monad.Extra

import           Data.Composition
import           Data.Maybe

import           Experiment.RABITT.Common

import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar
import           Maths.QuantumNumbers

import           QState
import           QState.Configure
import           QState.Energy
import           QState.OnePhoton
import           QState.PertWave
import           QState.TwoPhoton
import           QState.Units


calcAndSaveAtomicPhase :: QState()
calcAndSaveAtomicPhase = forGroundStates_
            $ (getReadOption "kappas2">>=).:calcAndSaveAtomicPhaseForQNums

calcAndSaveAtomicPhaseForQNums :: QNum -> QNum -> [QNum] -> QState()
calcAndSaveAtomicPhaseForQNums kappa0 n0 kappas2 = do
    irStepFraction <- getReadOption "IRStepFractionRABITT"
    es             <- getEs kappa0 n0>>=mapM toUnits
                        . map (setUnit Energy . fromReal) . drop irStepFraction
    phase          <- calcAtomicPhaseForQNums kappa0 n0 kappas2

    savePhase "Atomic" es phase


calcAtomicPhases :: QState [[Double]]
calcAtomicPhases = forGroundStates
                 $ (getReadOption "kappas1">>=).:calcAtomicPhaseForQNums

calcAtomicPhaseForQNums :: QNum -> QNum -> [QNum] -> QState [Double]
calcAtomicPhaseForQNums kappa0 n0 kappas2 = toPhaseOrDelay
    . ketElems . sum=<<join (sequence<$>mapM
            ((matElemPairedMj kappa0 n0<$>getReadOption "kappas1"??kappas2)??)
                (mValues . maximum $ map jFromKappa kappas2))

matElemPairedMj :: QNum -> QNum -> [QNum] -> [QNum] -> QNum -> QState Ket
matElemPairedMj kappa0 n0 kappas1 kappas2 mj = do
    kA <- getReadOption "eFinalIndexAbsRABITT"
            >>=matElemSingleMj kappa0 n0 kappas1 kappas2 mj>>=ketAbs
    kE <- getReadOption "eFinalIndexEmiRABITT"
            >>=matElemSingleMj kappa0 n0 kappas1 kappas2 mj>>=ketEmi
    return $ kzip (*) ((<|)kA) kE

matElemSingleMj :: QNum -> QNum -> [QNum] -> [QNum] -> QNum -> Int -> QState Ket
matElemSingleMj kappa0 n0 kappas1 kappas2 mj eFinalIndex = sum<$>sequence
    [ matElemSingleChannel kappa0 n0 kappas1 kappa2 mj eFinalIndex
                                                        | kappa2 <- kappas2 ]

matElemSingleChannel :: QNum -> QNum -> [QNum] -> QNum -> QNum -> Int
                                                                -> QState Ket
matElemSingleChannel kappa0 n0 kappas1 kappa2 mj eFinalIndex = do
        mEs  <- getMElements [kappa0] [n0] kappas1 [kappa2] [mj] eFinalIndex
        xi'' <- xi'<$>getReadOption "angleRABITT"
        getExcitedState kappa0 n0 $ map (*xi'') mEs
    where xi' mTheta | isJust mTheta = xi kappa2 mj (fromJust mTheta) 0
                     | otherwise     = 1
