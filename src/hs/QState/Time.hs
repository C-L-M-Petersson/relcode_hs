module QState.Time where

import           QState
import           QState.Time.Internal
import           QState.Units.Time


getTGrid :: QState [Double]
getTGrid = withCDict . tGrid=<<getTimeUnit
