module QState.Energy where

import           Control.Monad

import           Data.Composition

import           Maths.Interpolate
import           Maths.HilbertSpace

import           QState
import           QState.Energy.Internal


getXUV :: QState Pulse
getXUV = withCDict . xuv=<<getEnergyUnit

getXUVKetOnGrid :: [Double] -> QState Ket
getXUVKetOnGrid es = join $ withCDict . xuvKet es<$>getEnergyUnit

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

getEKinMin :: QState Double
getEKinMin = withCDict . eKinMin=<<getEnergyUnit

getEKinMax :: QState Double
getEKinMax = withCDict . eKinMax=<<getEnergyUnit



interpolateEnergyKet :: Ket -> QState Ket
interpolateEnergyKet = (<$>getNEs) . flip changeKetGridSize

energyKetToEkinGrid :: Ket -> QState Ket
energyKetToEkinGrid k = (`interpolateKet`k)
                                    <$>(getEnergyUnit>>=withCDict . eKinGrid)
