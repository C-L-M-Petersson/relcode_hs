{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Experiment.CrossSections.OnePhoton
(   crossSection1ph
,   crossSection1phForQNums

,   crossSectionPCurs
,   crossSectionPCur
) where

import           Control.Monad.Extra

import           Data.List.Extra

import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar
import           Maths.QuantumNumbers

import           QState
import           QState.Configure
import           QState.Energy
import           QState.OnePhoton
import           QState.Output
import           QState.Units
import           QState.Units.CrossSec
import           QState.Utility.Constants



crossSection1ph :: QState()
crossSection1ph = whenRunCrossSections1ph . join $ crossSection1phForQNums
    <$>getReadOption "kappas0"<*>getReadOption "ns0"<*>getReadOption "kappas1"

crossSection1phForQNums :: [QNum] -> [QNum] -> [QNum] -> QState()
crossSection1phForQNums kappas0 ns0 kappas1 = sequence_
        [ crossSectionPCurs kappa0 n0 kappas1>>=
                ifSaveData printQStateFileWithUnits "CrossSectionPCur"
        >>crossSectionAmps  kappa0 n0 kappas1>>=
                ifSaveData printQStateFileWithUnits "CrossSectionAmp"
            | (kappa0,n0) <- zip kappas0 ns0 ]
    where ifSaveData save key val = whenM (getReadOption ("save"++key++"1ph"))
                                  $ save ("outFile"++key++"1ph") val

whenRunCrossSections1ph :: QState() -> QState()
whenRunCrossSections1ph = whenM (getReadOption "runCrossSection1ph")


crossSectionPCurs :: QNum -> QNum -> [QNum] -> QState Ket
crossSectionPCurs kappa0 n0 kappas1 = sum
                                   <$>mapM (crossSectionPCur kappa0 n0) kappas1

crossSectionPCur :: QNum -> QNum -> QNum -> QState Ket
crossSectionPCur kappa0 n0 kappa1 = getPCur kappa0 n0 kappa1
    >>=getExcitedState kappa0 n0
                        . map (setUnit CrossSec . fromReal . (*(pi/3)) . negate)


crossSectionAmps :: QNum -> QNum -> [QNum] -> QState Ket
crossSectionAmps kappa0 n0 kappas1 = sum
                                  <$>mapM (crossSectionAmp kappa0 n0) kappas1

crossSectionAmp :: QNum -> QNum -> QNum -> QState Ket
crossSectionAmp kappa0 n0 kappa1 = zipWith calc<$>getAmp kappa0 n0 kappa1
                                               <*>getWaveNumbers kappa0 n0
                                                    >>=getExcitedState kappa0 n0
    where calc a k = setUnit CrossSec $ fromReal (2*pi/3*fsc*a**2*k)
