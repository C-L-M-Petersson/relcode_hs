module QState.Internal
(   System(System)
,   cDict_
,   eUnit_
,   tUnit_

,   twoPhoton_

,   omegasXUV_
) where

import           QState.Configure.Internal
import           QState.Units.Energy
import           QState.Units.Time


data System = System
              { cDict_     :: CDict
              , eUnit_     :: EnergyUnit
              , tUnit_     :: TimeUnit

              , twoPhoton_ :: Bool

              , omegasXUV_ :: [Double]
              }
