module QState.Units.Energy
(   EnergyUnit(AU,EV)
) where

import           QState.Units.Internal


data EnergyUnit = AU | EV

instance Read EnergyUnit where
    readsPrec _ ('a':'u':str) = [(AU,str)]
    readsPrec _ ('e':'V':str) = [(EV,str)]
    readsPrec _ str           = error $ "cannot read energy unit "++str

instance Show EnergyUnit where
    show AU = "au"
    show EV = "eV"

instance Unit EnergyUnit where
    toUnitFactor AU = 1
    toUnitFactor EV = 27.211396641308
