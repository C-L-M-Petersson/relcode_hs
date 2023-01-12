module QState.Units
(   from
,   to

,   EnergyUnit
,   toEnergyUnit
,   fromEnergyUnit
) where

import           QState
import           QState.Units.Energy
import           QState.Units.Internal


toEnergyUnit   :: (Floating a,Fractional a) => a -> QState a
toEnergyUnit   x = (`to`  x)<$>getEnergyUnit

fromEnergyUnit :: (Floating a,Fractional a) => a -> QState a
fromEnergyUnit x = (`from`x)<$>getEnergyUnit


toTimeUnit   :: (Floating a,Fractional a) => a -> QState a
toTimeUnit   x = (`to`  x)<$>getTimeUnit

fromTimeUnit :: (Floating a,Fractional a) => a -> QState a
fromTimeUnit x = (`from`x)<$>getTimeUnit
