module QState.Units.Internal where

import                QState.Configure.Internal


class Unit a where
    to   :: Fractional b => a -> b -> b
    from :: Fractional b => a -> b -> b



data EnergyUnit = AU | EV

instance Read EnergyUnit where
    readsPrec _ "au" = [(AU,"")]
    readsPrec _ "eV" = [(EV,"")]

instance Unit EnergyUnit where
    to   AU = id
    to   EV = (*27.211396641308)
    from AU = id
    from EV = (*0.0367493081366)
