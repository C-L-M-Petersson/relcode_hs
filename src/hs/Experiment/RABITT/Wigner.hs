module Experiment.RABITT.Wigner
(   calcAndSaveWignerPhase
,   calcAndSaveWignerPhaseForQNums

,   calcWignerPhases
,   calcWignerPhaseForQNums
) where

import           Data.Composition
import           Data.Tuple.HT

import           Experiment.RABITT.Common

import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar
import           Maths.QuantumNumbers

import           QState
import           QState.Configure
import           QState.OnePhoton
import           QState.Units


calcAndSaveWignerPhase :: QState()
calcAndSaveWignerPhase = forGroundStates_
            $ (getReadOption "kappas1">>=).:calcAndSaveWignerPhaseForQNums

calcAndSaveWignerPhaseForQNums :: QNum -> QNum -> [QNum] -> QState()
calcAndSaveWignerPhaseForQNums kappa0 n0 kappas1 = withOptions
    ["kappas0","ns0","kappas1"] [show [kappa0],show [n0],show kappas1] $ do
    irStepFraction <- getReadOption "IRStepFractionRABITT"
    es             <- getEs kappa0 n0>>=mapM toUnits
                        . map (setUnit Energy . fromReal) . drop irStepFraction
    wignerPhase    <- calcWignerPhaseForQNums kappa0 n0 kappas1

    savePhase "Wigner" es wignerPhase


calcWignerPhases :: QState [[Double]]
calcWignerPhases = forGroundStates
                 $ (getReadOption "kappas1">>=).:calcWignerPhaseForQNums

calcWignerPhaseForQNums :: QNum -> QNum -> [QNum] -> QState [Double]
calcWignerPhaseForQNums kappa0 n0 kappas1 = toPhaseOrDelay
        . ketElems . sum=<<sequence [ matElemPairedChannel kappa0 n0 kappa1 mJ
            | kappa1 <- kappas1, mJ <- mValuesKappas [[kappa0],kappas1]]

matElemPairedChannel :: QNum -> QNum -> QNum -> QNum -> QState Ket
matElemPairedChannel kappa0 n0 kappa1 mJ = uncurry (kzip (*)) . mapFst (<|)
        <$>(matElemSingleChannel kappa0 n0 kappa1 mJ>>=ketAbsEmi)

matElemSingleChannel :: QNum -> QNum -> QNum -> QNum -> QState Ket
matElemSingleChannel kappa0 n0 kappa1 mJ = getExcitedState kappa0 n0
                                            =<<getMatElem kappa0 n0 kappa1 mJ
