module QState.Utility.Internal
(   fsc
,   byFsc

,   waveNumber
,   coulombPhase
) where

import           Data.Complex

import           Math.Gamma
import           Maths.QuantumNumbers


fsc :: Double
fsc = 0.0072973525693

byFsc :: Double
byFsc = 137.0359990836958


waveNumber :: Double -> Double
waveNumber e = sqrt((e*fsc + byFsc)**2 - byFsc**2) --Only relativistic

coulombPhase :: QNum -> Double -> Double
coulombPhase kappa e = phase(gamma((l+1):+(-z/k))) -l*pi/2--Should the -l*pi/2 be moved?
    where
        z = 1
        l = doubleFromQNum $ lFromKappa kappa
        k = waveNumber e
