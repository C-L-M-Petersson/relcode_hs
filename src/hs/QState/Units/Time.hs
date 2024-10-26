module QState.Units.Time
(   TimeUnit(..)
,   auToAs
) where

import           QState.Units.Internal


data TimeUnit = AU | S | MS | US | NS | PS | FS | AS

instance Show TimeUnit where
    show AU = "au"
    show S  = "s"
    show MS = "ms"
    show US = "µs"
    show NS = "ns"
    show PS = "ps"
    show FS = "fs"
    show AS = "as"

instance Read TimeUnit where
    readsPrec _ ('a':'u':str) = [(AU,str)]
    readsPrec _ (    's':str) = [(S ,str)]
    readsPrec _ ('m':'s':str) = [(MS,str)]
    readsPrec _ ('µ':'s':str) = [(US,str)]
    readsPrec _ ('n':'s':str) = [(NS,str)]
    readsPrec _ ('p':'s':str) = [(PS,str)]
    readsPrec _ ('f':'s':str) = [(FS,str)]
    readsPrec _ ('a':'s':str) = [(AS,str)]
    readsPrec _  str          = error $ "cannot read time unit "++str

instance Unit TimeUnit where
    toUnitFactor AU = 1
    toUnitFactor S  = auToAs*10**(-18)
    toUnitFactor MS = auToAs*10**(-15)
    toUnitFactor US = auToAs*10**(-12)
    toUnitFactor NS = auToAs*10**(-9)
    toUnitFactor PS = auToAs*10**(-6)
    toUnitFactor FS = auToAs*10**(-3)
    toUnitFactor AS = auToAs


auToAs :: Double
auToAs = 24.188843265857
