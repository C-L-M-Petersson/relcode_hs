module QState.Units.Internal where

import           QState.Configure.Internal


class Unit a where
    {-# MINIMAL (toUnitFactor | fromUnitFactor) #-}
    toUnitFactor :: (Floating b,Fractional b) => a -> b
    fromUnitFactor :: (Floating b,Fractional b) => a -> b

    {-# INLINE toUnitFactor   #-}
    {-# INLINE fromUnitFactor #-}
    toUnitFactor   u = 1/fromUnitFactor u
    fromUnitFactor u = 1/  toUnitFactor u



to :: (Unit a,Floating b,Fractional b) => a -> b -> b
to u = (toUnitFactor u*)

from :: (Unit a,Floating b,Fractional b) => a -> b -> b
from u = (fromUnitFactor u*)
