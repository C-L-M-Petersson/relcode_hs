module QState.Time
(   getTGrid
) where

import           QState
import           QState.Time.Internal


getTGrid :: QState [Double]
getTGrid = withCDict . tGrid=<<getTimeUnit
