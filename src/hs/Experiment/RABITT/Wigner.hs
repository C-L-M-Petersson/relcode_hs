module Experiment.RABITT.Wigner
(   calcAndSaveWignerPhase
,   calcAndSaveWignerPhaseForQNums

,   calcWignerPhases
,   calcWignerPhaseForQNums
) where

import           Control.Monad.Extra

import           Data.Composition
import           Data.Maybe
import           Data.Tuple.HT

import           Experiment.CrossSections.OnePhoton
import           Experiment.RABITT.Common

import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar
import           Maths.QuantumNumbers

import           QState
import           QState.Configure
import           QState.OnePhoton
import           QState.PertWave
import           QState.Units


calcAndSaveWignerPhase :: QState()
calcAndSaveWignerPhase = forGroundStates_
            $ (getReadOption "kappas1">>=).:calcAndSaveWignerPhaseForQNums

calcAndSaveWignerPhaseForQNums :: QNum -> QNum -> [QNum] -> QState()
calcAndSaveWignerPhaseForQNums kappa0 n0 kappas1 = do
    irStepFraction <- getReadOption "IRStepFractionRABITT"
    es             <- getEs kappa0 n0>>=mapM toUnits
                        . map (setUnit Energy . fromReal) . drop irStepFraction
    phase          <- calcWignerPhaseForQNums kappa0 n0 kappas1

    savePhase "Wigner" es phase


calcWignerPhases :: QState [[Double]]
calcWignerPhases = forGroundStates
                 $ (getReadOption "kappas1">>=).:calcWignerPhaseForQNums

calcWignerPhaseForQNums :: QNum -> QNum -> [QNum] -> QState [Double]
calcWignerPhaseForQNums kappa0 n0 kappas1 = toPhaseOrDelay
    . ketElems . sum=<<mapM (matElemPairedMj kappa0 n0 kappas1)
                            (mValues . maximum $ map jFromKappa kappas1)

matElemPairedMj :: QNum -> QNum -> [QNum] -> QNum -> QState Ket
matElemPairedMj kappa0 n0 kappas1 mj = uncurry (kzip (*)) . mapFst (<|)
        <$>(matElemSingleMj kappa0 n0 kappas1 mj>>=ketAbsEmi)

matElemSingleMj :: QNum -> QNum -> [QNum] -> QNum -> QState Ket
matElemSingleMj kappa0 n0 kappas1 mj = sum<$>sequence
    [ matElemSingleChannel kappa0 n0 kappa1 mj | kappa1 <- kappas1 ]

matElemSingleChannel :: QNum -> QNum -> QNum -> QNum -> QState Ket
matElemSingleChannel kappa0 n0 kappa1 mj = do
        mEs  <- getMatElem kappa0 n0 kappa1
        xi'' <- xi'<$>getReadOption "angleRABITT"
        getExcitedState kappa0 n0 $ map (*xi'') mEs
    where xi' mTheta | isJust mTheta = xi kappa1 mj (fromJust mTheta) 0
                     | otherwise     = 1
