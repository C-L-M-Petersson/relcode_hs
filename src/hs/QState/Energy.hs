module QState.Energy where

import           Control.Monad

import           Data.Composition

import           Maths.HilbertSpace
import           Maths.Interpolate

import           QState
import           QState.Energy.Internal
import           QState.Units.Internal


getXUV :: QState Pulse
getXUV = withCDict . xuv=<<getEnergyUnit

getXUVKetOnGrid :: [Double] -> QState Ket
getXUVKetOnGrid es = getEnergyUnit>>=withCDict . xuvKet es

getXUVKet :: QState Ket
getXUVKet = getOmegasXUV>>=getXUVKetOnGrid

getInterpolatedXUVKet :: QState Ket
getInterpolatedXUVKet = (e_s<$>getOmegasXUV<*>getNEs)>>=getXUVKetOnGrid
    where e_s es e_N = let de_   = (e_Max-e_Min)/(fromIntegral e_N-1)
                           e_Min = head es
                           e_Max = last es
                        in takeWhile (<e_Max) $ map ((+e_Min) . (*de_)) [0..]



getNEs :: QState Int
getNEs = withCDict nEs

getEGridMin :: QState Double
getEGridMin = withCDict . eGridMin=<<getEnergyUnit

getEGridMax :: QState Double
getEGridMax = withCDict . eGridMax=<<getEnergyUnit

getEGrid :: QState [Double]
getEGrid = withCDict . eGrid=<<getEnergyUnit



interpolateEnergyKet :: Ket -> QState Ket
interpolateEnergyKet = (<$>getNEs) . flip changeKetGridSize

energyKetToEGrid :: Ket -> QState Ket
energyKetToEGrid k = (`interpolateKet`k)<$>(getEnergyUnit>>=withCDict . eGrid)



interpolatedExcitedState :: [Double] -> [Scalar] -> QState Ket
interpolatedExcitedState es vs = (*)<$>getInterpolatedXUVKet
                        <*>interpolateEnergyKet (Ket (Just Energy) (Just es) vs)