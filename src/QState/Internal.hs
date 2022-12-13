module QState.Internal where

import           QState.Internal.Configure


data System = System
              { cDict     :: CDict
              , twoPhoton :: Bool

              , omegasXUV :: [Double]
              }
