module QState.Internal where

import           QState.Configure.Internal
import           QState.Units.Energy
import           QState.Units.Time


data System = System
              { cDict     :: CDict
              , eUnit     :: EnergyUnit
              , tUnit     :: TimeUnit

              , twoPhoton :: Bool

              , omegasXUV :: [Double]
              }
