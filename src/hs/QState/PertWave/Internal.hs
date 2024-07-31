module QState.PertWave.Internal
(   subtractedPhaseFactor
,   subtractedPhase

,   xi
) where

import           Data.Composition

import           Math.SphericalHarmonics.AssociatedLegendre
import           Maths.HilbertSpace
import           Maths.QuantumNumbers
import           Maths.WignerSymbol

import           QState.PertWave.NonRelativistic as NonRel
--import           QState.PertWave.Relativistic    as Rel


subtractedPhaseFactor :: QNum -> Double -> Double -> Scalar
subtractedPhaseFactor = exp . fromImag .:. subtractedPhase

subtractedPhase :: QNum -> Double -> Double -> Double
subtractedPhase kappa zEff eKin = -pi*l/2 - NonRel.coulombPhase kappa zEff eKin
    where l = doubleFromQNum $ lFromKappa kappa


xi :: QNum -> QNum -> Double -> Double -> Scalar
xi kappa mj theta phi = sum [ sphericalHarmonic l ml theta phi
                                * clebschGordan l ml (1/2) ms j mj
                                    | ml <- mValues l
                                    , ms <- mValues (1/2)
                                    ]
    where
        l = lFromKappa kappa
        j = jFromKappa kappa

sphericalHarmonic :: QNum -> QNum -> Double -> Double -> Scalar
sphericalHarmonic l m theta phi = exp( i(m''*phi) )
                        * fromReal(n * associatedLegendreWrapper m')
    where
        l'  = intFromQNum l
        m'  = intFromQNum m
        l'' = doubleFromQNum l
        m'' = doubleFromQNum m
        n = sqrt( ((2*l''+1)*product [1..l''-m''])
                / ( 4*pi    *product [1..l''+m''])
                )

        associatedLegendreWrapper :: Int -> Double
        associatedLegendreWrapper ml
            | ml<0      = fact*associatedLegendreWrapper (-ml)
            | otherwise = associatedLegendreFunction l' ml theta
            where
                ml' = fromIntegral ml
                fact = (-1)**ml'*product [1..l''-ml']/product [1..l''+ml']
