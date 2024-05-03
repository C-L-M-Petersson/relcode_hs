module QState.PertWave.NonRelativistic
(   coulombPhase
,   waveNumber
) where

import           Data.Complex

import           Math.Gamma
import           Maths.QuantumNumbers


coulombPhase :: QNum -> Double -> Double -> Double
coulombPhase kappa zEff eKin = phase( gamma( (l+1) :+ (zEff/k) ) )
    where
        l = doubleFromQNum $ lFromKappa kappa
        k = waveNumber eKin

waveNumber :: Double -> Double
waveNumber eKin = sqrt(2*eKin)
