module QState.PertWave.Internal
(   subtractedPhaseFactor
,   subtractedPhase

,   xi
) where

import           Control.Monad.Extra

import           Data.Complex

import           Math.SphericalHarmonics.AssociatedLegendre
import           Maths.HilbertSpace
import           Maths.QuantumNumbers
import           Maths.WignerSymbol

import           QState
import           QState.Configure
import           QState.PertWave.NonRelativistic as NonRel
import           QState.PertWave.Relativistic    as Rel


subtractedPhaseFactor :: QNum -> Double -> Double -> Scalar
subtractedPhaseFactor kappa zEff eKin = exp . fromImag
                                      $ subtractedPhase kappa zEff eKin

subtractedPhase :: QNum -> Double -> Double -> Double
subtractedPhase kappa zEff eKin = -pi*l/2 - NonRel.coulombPhase kappa eKin zEff
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
        [l' ,m' ] = map intFromQNum    [l,m]
        [l'',m''] = map doubleFromQNum [l,m]
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
