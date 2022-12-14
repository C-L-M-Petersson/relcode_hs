module QState.Light where

import           Control.Monad

import           Data.Composition

import           Maths.HilbertSpace

import           QState
import           QState.Light.Internal


getXUV :: QState Pulse
getXUV = withCDict=<<xuv<$>getEnergyUnit

getXUVKet :: QState Ket
getXUVKet = join $ withCDict .: xuvKet<$>getOmegasXUV<*>getEnergyUnit
