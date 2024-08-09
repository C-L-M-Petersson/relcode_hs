module QState.TwoPhoton
(   getEnergyRPA
,   getEnergyFin

,   getIROmegas

,   getMElement
,   getMElements

,   getMElementCorrection
,   getMElementsCorrection

,   getMElementCorrected
,   getMElementsCorrected
) where

import           Data.Composition
import           Data.List                 (transpose)

import           Maths.HilbertSpace.Scalar
import           Maths.QuantumNumbers

import           QState
import           QState.TwoPhoton.Internal
import           QState.TwoPhoton.RPAE


getEnergyRPA :: QNum -> QNum -> QState [Double]
getEnergyRPA = withCDictM.:energyRPA

getEnergyFin :: QNum -> QNum -> Int -> QState [Double]
getEnergyFin = withCDictM.:.energyFin


getIROmegas :: Int -> QState [Double]
getIROmegas = withCDictM . irOmegas


getMElement :: QNum -> QNum -> QNum -> QNum -> QNum -> Int -> QState [Scalar]
getMElement = withCDictM.:::mElement

getMElements :: [QNum] -> [QNum] -> [QNum] -> [QNum] ->  [QNum] -> Int
                                                            -> QState [Scalar]
getMElements kappas0 ns0 kappas1 kappas2 mJs eFinalIndex = map sum . transpose
    <$>sequence [ getMElement kappa0 n0 kappa1 kappa2 mJ eFinalIndex
                    | (kappa0,n0) <- zip kappas0 ns0, kappa1 <- kappas1
                                                    , kappa2 <- kappas2
                                                    , mJ     <- mJs ]

getMElementCorrection :: QNum -> QNum -> QNum ->  QNum -> Int
                                                            -> QState [Scalar]
getMElementCorrection kappa0 n0 kappa2 mJ eFinalIndex = withCDictM
                    $ mElementCorrectionKappa kappa0 n0 kappa2 mJ eFinalIndex

getMElementsCorrection :: [QNum] -> [QNum] -> [QNum] ->  [QNum] -> Int
                                                            -> QState [Scalar]
getMElementsCorrection kappas0 ns0 kappas2 mJs eFinalIndex = map sum
                                                                   . transpose
    <$>sequence [ getMElementCorrection kappa0 n0 kappa2 mJ eFinalIndex
        | (kappa0,n0) <- zip kappas0 ns0, kappa2 <- kappas2, mJ <- mJs ]


getMElementCorrected :: QNum -> QNum -> QNum -> QNum -> QNum -> Int
                                                            -> QState [Scalar]
getMElementCorrected kappa0 n0 kappa1 kappa2 mJ eFinalIndex = zipWith (+)
                <$>getMElement           kappa0 n0 kappa1 kappa2 mJ eFinalIndex
                <*>getMElementCorrection kappa0 n0        kappa2 mJ eFinalIndex

getMElementsCorrected :: [QNum] -> [QNum] -> [QNum] -> [QNum] ->  [QNum] -> Int
                                                            -> QState [Scalar]
getMElementsCorrected kappas0 ns0 kappas1 kappas2 mJs eFinalIndex = map sum . transpose
    <$>sequence [ getMElementCorrected kappa0 n0 kappa1 kappa2 mJ eFinalIndex
                    | (kappa0,n0) <- zip kappas0 ns0, kappa1 <- kappas1
                                                    , kappa2 <- kappas2
                                                    , mJ     <- mJs ]
--getMElementsCorrected kappas0 ns0 kappas1 kappas2 mJs eFinalIndex = do
--getMElementsCorrected kappas0 ns0 kappas1 kappas2 mJs eFinalIndex = do
--       mes <- getMElementsCorrection kappas0 ns0         kappas2 mJs eFinalIndex
--       error $ show mes
--getMElementsCorrected kappas0 ns0 kappas1 kappas2 mJs eFinalIndex = zipWith (+)
--getMElementsCorrected kappas0 ns0 kappas1 kappas2 mJs eFinalIndex = zipWith (\m mCorr -> mCorr)
--    <$>getMElements           kappas0 ns0 kappas1 kappas2 mJs eFinalIndex
--    <*>getMElementsCorrection kappas0 ns0         kappas2 mJs eFinalIndex
