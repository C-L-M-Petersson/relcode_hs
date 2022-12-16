module QState.FilePath where

import           Data.Composition

import           Maths.QuantumNumbers

import           QState
import           QState.FilePath.Internal


getCDictFilePath :: String -> QState FilePath
getCDictFilePath = withCDict . cDictFilePath

createParentDir :: String -> QState()
createParentDir = withCDictM . createCDictParentDir



getRunDir :: QState FilePath
getRunDir = withCDict runDir

getHFDir :: QState FilePath
getHFDir = withCDict hfDir

getPertDir :: QNum -> QNum -> QState FilePath
getPertDir = withCDict.:pertDir

getSecondPhotonDir :: QState FilePath
getSecondPhotonDir = withCDict secondPhotonDir
