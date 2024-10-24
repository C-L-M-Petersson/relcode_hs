module QState.PertWave.Relativistic
(   coulombPhase
,   waveNumber
,   phaseGamma
) where

import           Data.Complex

import           Math.Gamma
import           Maths.QuantumNumbers

import           QState.Utility.Constants


coulombPhase :: QNum -> Double -> Double -> Double
coulombPhase kappa zEff eKin = phase(gamma(phaseGamma kappa zEff :+ (zEff/k)))
    where k = waveNumber eKin

waveNumber :: Double -> Double
waveNumber eKin = sqrt( (eKin*fsc + byFsc)**2 - byFsc**2 )

phaseGamma :: QNum -> Double -> Double
phaseGamma kappa zEff = sqrt( doubleFromQNum kappa**2 - fsc**2*zEff**2 )
