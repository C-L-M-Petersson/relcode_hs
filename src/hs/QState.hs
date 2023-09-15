module QState
(   QState

,   liftIO

,   getCDict
,   withCDict
,   withCDictM

,   getUnit
,   getEnergyUnit
,   getTimeUnit

,   getOmegasXUV
) where

import           Control.Monad.State

import           QState.Configure.Internal
import           QState.Internal
import           QState.Units.Energy
import           QState.Units.Internal
import           QState.Units.Time


type QState a = StateT System IO a

getCDict :: QState CDict
getCDict = gets cDict_

withCDict  :: (CDict -> a) -> QState a
withCDict  = (<$>getCDict)

withCDictM :: (CDict -> IO a) -> QState a
withCDictM x = liftIO . x=<<getCDict


getUnit :: UnitType -> QState GenericUnit
getUnit Energy = toGeneric<$>getEnergyUnit
getUnit Time   = toGeneric<$>getTimeUnit

getEnergyUnit :: QState EnergyUnit
getEnergyUnit = gets eUnit_

getTimeUnit :: QState TimeUnit
getTimeUnit = gets tUnit_


getOmegasXUV :: QState [Double]
getOmegasXUV = gets omegasXUV_
