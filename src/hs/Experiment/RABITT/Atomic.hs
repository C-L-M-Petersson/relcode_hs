module Experiment.RABITT.Atomic
(   calcAndSaveAtomicPhase
,   calcAndSaveAtomicPhaseGroundState
,   calcAtomicPhaseGroundState
) where

import           Experiment.RABITT.Common

import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar
import           Maths.QuantumNumbers

import           QState
import           QState.Configure
import           QState.OnePhoton
import           QState.TwoPhoton
import           QState.Units



calcAndSaveAtomicPhase :: QState()
calcAndSaveAtomicPhase = forGroundStates_ calcAndSaveAtomicPhaseGroundState

calcAndSaveAtomicPhaseGroundState :: QNum -> QNum -> QState()
calcAndSaveAtomicPhaseGroundState kappa0 n0 = do
    irStepFraction <- getReadOption "IRStepFractionRABITT"
    es             <- getEs kappa0 n0>>=mapM toUnits
                        . map (setUnit Energy . fromReal) . drop irStepFraction
    atomicPhase    <- calcAtomicPhaseGroundState kappa0 n0

    savePhase "Atomic" es atomicPhase

calcAtomicPhaseGroundState :: QNum -> QNum -> QState [Double]
calcAtomicPhaseGroundState kappa0 n0 = do
    kappas1 <- getReadOption "kappas1"
    kappas2 <- getReadOption "kappas2"

    toPhaseOrDelay=<<ketElems . sum . concat<$>sequence
        [ matElemPairedMj kappa0 n0 mJ
            | mJ <- mValuesKappas (kappa0:kappas1++kappas2) ]

matElemPairedMj :: QNum -> QNum -> QNum -> QState [Ket]
matElemPairedMj kappa0 n0 mJ = do
    kappas2 <- getReadOption "kappas2"

    kAs <- matElem "Abs" kappas2>>=mapM(getExcitedState kappa0 n0)>>=mapM ketAbs
    kEs <- matElem "Emi" kappas2>>=mapM(getExcitedState kappa0 n0)>>=mapM ketEmi

    return $ zipWith (\kA kE -> kzip (*) ((<|)kA) kE) kAs kEs
    where
        matElem :: String -> [QNum] -> QState [[Scalar]]
        matElem absEmi kappas2 = getMElements2ph kappa0 n0 kappas2 mJ
            =<<getReadOption ("eFinalIndex"++absEmi++"RABITT")
