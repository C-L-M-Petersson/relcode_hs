module QState.Internal
(   System(System)
,   cDict_

,   csUnit_
,   eUnit_
,   tUnit_
,   phiUnit_

,   twoPhoton_

,   omegasXUV_
) where

import           QState.Configure.Internal
import           QState.Units.CrossSec
import           QState.Units.Delay
import           QState.Units.Energy
import           QState.Units.Time


data System = System
              { cDict_     :: CDict

              , csUnit_    :: CrossSecUnit
              , eUnit_     :: EnergyUnit
              , tUnit_     :: TimeUnit
              , phiUnit_   :: Double -> DelayUnit

              , twoPhoton_ :: Bool

              , omegasXUV_ :: [Double]
              }
