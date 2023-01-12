module QState.Units.Energy where

import           QState.Units.Internal


data EnergyUnit = AU | EV

instance Read EnergyUnit where
    readsPrec _ ('a':'u':str) = [(AU,str)]
    readsPrec _ ('e':'V':str) = [(EV,str)]

instance Unit EnergyUnit where
    toUnitFactor AU = 1
    toUnitFactor EV = 27.211396641308
