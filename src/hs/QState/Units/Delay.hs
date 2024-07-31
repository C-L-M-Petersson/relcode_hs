{-# LANGUAGE FlexibleInstances #-}
module QState.Units.Delay
(   DelayUnit(..)
) where

import           QState.Units.Internal
import           QState.Units.Time (auToAs)


data DelayUnit = Rad | AS Double

instance Show DelayUnit where
    show Rad    = "rad"
    show (AS _) = "as"

instance Read (Double -> DelayUnit) where
    readsPrec _ ('r':'a':'d':str) = [(const Rad,str)]
    readsPrec _ ('a':'s'    :str) = [(AS       ,str)]
    readsPrec _  str              = error $ "could not read delay unit "++str

instance Unit DelayUnit where
    toUnitFactor  Rad       = 1
    toUnitFactor (AS omega) = 1/2/abs omega*auToAs
