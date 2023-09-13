module QState.Time.Internal where

import           QState.Configure.Internal
import           QState.Units.Time
import           QState.Units.Internal


nTs :: CDict -> Int
nTs = cDictReadOption "nTs"

tMin :: TimeUnit -> CDict -> Double
tMin tu cDict = tu`from`cDictReadOption "tMin" cDict

tMax :: TimeUnit -> CDict -> Double
tMax tu cDict = tu`from`cDictReadOption "tMax" cDict

tGrid :: TimeUnit -> CDict -> [Double]
tGrid tu cDict = takeWhile (<=tMax_) $ map ((+tMin_) . (*dt)) [0..]
    where
        dt    = (tMax_-tMin_)/(fromIntegral tN-1)
        tN    = nTs cDict
        tMin_ = tMin tu cDict
        tMax_ = tMax tu cDict
