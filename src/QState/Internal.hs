module QState.Internal where

import           QState.Configure
import           QState.Units.Internal


data System = System
              { cDict     :: CDict
              , eUnit     :: EnergyUnit

              , twoPhoton :: Bool

              , omegasXUV :: [Double]
              }
