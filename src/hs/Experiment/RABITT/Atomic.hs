module Experiment.RABITT.Atomic
(   calcAndSaveAtomicPhase
,   calcAndSaveAtomicPhaseForQNums

,   calcAtomicPhases
,   calcAtomicPhaseForQNums
) where

import           Data.Composition
import           Data.Maybe

import           Experiment.RABITT.Common

import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar
import           Maths.QuantumNumbers

import           QState
import           QState.Configure
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
    atomicPhase    <- calcAtomicPhaseForQNums kappa0 n0 kappas2

    savePhase "Atomic" es atomicPhase


calcAtomicPhases :: QState [[Double]]
calcAtomicPhases = forGroundStates
                 $ (getReadOption "kappas2">>=).:calcAtomicPhaseForQNums

calcAtomicPhaseForQNums :: QNum -> QNum -> [QNum] -> QState [Double]
calcAtomicPhaseForQNums kappa0 n0 kappas2 = do
    kappas1 <- getReadOption "kappas1"

    toPhaseOrDelay=<<ketElems . sum<$>sequence
        [ matElemPairedMj kappa0 n0 kappas1 kappa2 mJ
            | kappa2 <- kappas2
            , mJ     <- mValuesKappas (kappa0:kappas1++kappas2)
            ]


matElemPairedMj :: QNum -> QNum -> [QNum] -> QNum -> QNum -> QState Ket
matElemPairedMj kappa0 n0 kappas1 kappa2 mJ = do
    kA <- getReadOption "eFinalIndexAbsRABITT"
            >>=matElemSingleChannel kappa0 n0 kappas1 kappa2 mJ>>=ketAbs
    kE <- getReadOption "eFinalIndexEmiRABITT"
            >>=matElemSingleChannel kappa0 n0 kappas1 kappa2 mJ>>=ketEmi
    return $ kzip (*) ((<|)kA) kE

matElemSingleChannel :: QNum -> QNum -> [QNum] -> QNum -> QNum -> Int
                                                                -> QState Ket
matElemSingleChannel kappa0 n0 kappas1 kappa2 mJ eFinalIndex = do
        mEs  <- getMElements [kappa0] [n0] kappas1 [kappa2] [mJ] eFinalIndex
        xi'' <- xi'<$>getReadOption "angleRABITT"
        getExcitedState kappa0 n0 $ map (*xi'') mEs
    where xi' mTheta | isJust mTheta = xi kappa2 mJ (fromJust mTheta) 0
                     | otherwise     = 1
