module QState.Directories where

import           Data.Composition

import           Maths.QuantumNumbers

import           QState
import           QState.Directories.Internal


getRunDir :: QState FilePath
getRunDir = withCDict runDir

getHFDir :: QState FilePath
getHFDir = withCDict hfDir

getPertDir :: QNum -> QNum -> QState FilePath
getPertDir = withCDict.:pertDir
