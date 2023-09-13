module QState.HartreeFock where

import           Data.Composition

import           Maths.QuantumNumbers

import           QState
import           QState.HartreeFock.Internal


getHFEnergy :: QNum -> QNum -> QState Double
getHFEnergy = withCDictM.:hfEnergy
