module QState.Units
(   EnergyUnit
,   TimeUnit
,   UnitType
,   to
,   from

,   HasUnit
,   unitType
,   toUnits
,   fromUnits
,   setUnit

,   UnitType(..)
) where

import           QState
import           QState.Units.Energy
import           QState.Units.Internal
import           QState.Units.Time

class HasUnit a where
    unitType  :: a -> Maybe UnitType
    toUnits   :: a -> QState a
    fromUnits :: a -> QState a
    setUnit   :: UnitType -> a -> a
