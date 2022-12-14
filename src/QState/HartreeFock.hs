module QState.HartreeFock where

import           Data.Composition

import           Maths.QuantumNumbers

import           QState
import           QState.HartreeFock.Internal
import           QState.OnePhoton.Internal


getHFEnergy :: QNum -> QNum -> QState Double
getHFEnergy = withCDictM.:hfEnergy



getGroundStateShiftedOmegas :: QNum -> QNum -> QState [Double]
getGroundStateShiftedOmegas kappa0 n0 = withCDictM (omegas kappa0 n0)
--getGroundStateShiftedOmegas kappa0 n0 = withCDictM (omegas kappa0 n0)>>=
--    withCDictM . groundStateShift kappa0 n0
