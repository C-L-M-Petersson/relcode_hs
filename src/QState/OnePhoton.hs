module QState.OnePhoton where

import           Data.Composition

import           Maths.QuantumNumbers

import           QState
import           QState.OnePhoton.Internal


getOmegas :: QNum -> QNum -> QState [Double]
getOmegas = withCDictM.:omegas

getAmps   :: QNum -> QNum -> QNum -> QState [Double]
getAmps   = withCDictM.:.amps

getPhaseF :: QNum -> QNum -> QNum -> QState [Double]
getPhaseF = withCDictM.:.phaseF

getPhaseG :: QNum -> QNum -> QNum -> QState [Double]
getPhaseG = withCDictM.:.phaseG
