module QState.Units.Internal
(   Unit
,   toGeneric
,   toUnitFactor
,   fromUnitFactor

,   GenericUnit
,   unitI

,   to
,   from

,   UnitType(Energy,Time)
) where


class Unit a where
    {-# MINIMAL (toUnitFactor | fromUnitFactor) #-}
    toGeneric :: a -> GenericUnit
    toUnitFactor   :: a -> Double
    fromUnitFactor :: a -> Double

    {-# INLINE toGeneric #-}
    toGeneric = GenericUnit . toUnitFactor

    {-# INLINE toUnitFactor   #-}
    {-# INLINE fromUnitFactor #-}
    toUnitFactor   u = 1/fromUnitFactor u
    fromUnitFactor u = 1/  toUnitFactor u


data GenericUnit = GenericUnit { fact_ :: Double } deriving(Show)

instance Unit GenericUnit where
    toGeneric                       = id
    toUnitFactor (GenericUnit fact) = fact

unitI :: GenericUnit
unitI = GenericUnit 1


to :: Unit a => a -> Double -> Double
to u = (toUnitFactor u*)

from :: Unit a => a -> Double -> Double
from u = (fromUnitFactor u*)


data UnitType = Energy | Time
