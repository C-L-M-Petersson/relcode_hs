module QState.TwoPhoton
(   getEnergyRPA
,   getEnergyFin

,   getMElement
,   getMElements
) where

import           Data.Composition
import           Data.List                 (transpose)

import           Maths.HilbertSpace.Scalar
import           Maths.QuantumNumbers

import           QState
import           QState.TwoPhoton.Internal


getEnergyRPA :: QNum -> QNum -> QState [Double]
getEnergyRPA = withCDictM.:energyRPA

getEnergyFin :: QNum -> QNum -> Int -> QState [Double]
getEnergyFin = withCDictM.:.energyFin


getMElement :: QNum -> QNum -> QNum -> QNum -> QNum -> Int -> QState [Scalar]
getMElement = withCDictM.:::mElement

getMElements :: [QNum] -> [QNum] -> [QNum] -> [QNum] ->  [QNum] -> Int
                                                            -> QState [Scalar]
getMElements kappas0 ns0 kappas1 kappas2 msJ eFinalIndex = map sum . transpose
    <$>sequence [ getMElement kappa0 n0 kappa1 kappa2 mJ eFinalIndex
                    | (kappa0,n0) <- zip kappas0 ns0, kappa1 <- kappas1
                                                    , kappa2 <- kappas2
                                                    , mJ     <- msJ
                                                ]
