module QState.TwoPhoton where

import           Data.Composition

import           Maths.QuantumNumbers

import           QState
import           QState.Energy
import           QState.TwoPhoton.Internal


getEnergyRPA :: QNum -> QNum -> QState [Double]
getEnergyRPA = withCDictM.:energyRPA

getEnergyFin :: QNum -> QNum -> Int -> QState [Double]
getEnergyFin = withCDictM.:.energyFin

