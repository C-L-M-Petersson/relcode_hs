module QState.Units where

import           QState
import           QState.Units.Internal


toEnergyUnit   :: Fractional a => a -> QState a
toEnergyUnit   x = (`to`  x)<$>getEnergyUnit

fromEnergyUnit :: Fractional a => a -> QState a
fromEnergyUnit x = (`from`x)<$>getEnergyUnit
