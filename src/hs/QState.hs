module QState
(   QState

,   liftIO

,   getCDict
,   withCDict
,   withCDictM

,   getUnit
,   getEnergyUnit
,   getTimeUnit
,   getDelayUnit

,   getOmegasXUV
) where

import           Control.Lens
import           Control.Monad.State

import           QState.Configure.Internal
import           QState.Internal
import           QState.Units.CrossSec
import           QState.Units.Delay
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
getUnit  CrossSec     = toGeneric<$> getCrossSecUnit
getUnit (Delay omega) = toGeneric<$>(getDelayUnit??omega)
getUnit  Energy       = toGeneric<$> getEnergyUnit
getUnit  Time         = toGeneric<$> getTimeUnit

getCrossSecUnit :: QState CrossSecUnit
getCrossSecUnit = gets csUnit_

getEnergyUnit   :: QState EnergyUnit
getEnergyUnit   = gets eUnit_

getTimeUnit     :: QState TimeUnit
getTimeUnit     = gets tUnit_

getDelayUnit    :: QState (Double -> DelayUnit)
getDelayUnit    = gets phiUnit_


getOmegasXUV :: QState [Double]
getOmegasXUV = gets omegasXUV_
