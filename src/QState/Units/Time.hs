module QState.Units.Time where

import           QState.Units.Internal


data TimeUnit = AU | S | MS | US | NS | PS | FS | AS

instance Read TimeUnit where
    readsPrec _ ('a':'u':str) = [(AU,str)]
    readsPrec _ (    's':str) = [(S ,str)]
    readsPrec _ ('m':'s':str) = [(MS,str)]
    readsPrec _ ('µ':'s':str) = [(US,str)]
    readsPrec _ ('n':'s':str) = [(NS,str)]
    readsPrec _ ('p':'s':str) = [(PS,str)]
    readsPrec _ ('f':'s':str) = [(FS,str)]
    readsPrec _ ('a':'s':str) = [(AS,str)]

instance Unit TimeUnit where
    toUnitFactor AU = 1
    toUnitFactor S  = 24.188843265857*10**(-18)
    toUnitFactor MS = 24.188843265857*10**(-15)
    toUnitFactor US = 24.188843265857*10**(-12)
    toUnitFactor NS = 24.188843265857*10**(-9)
    toUnitFactor PS = 24.188843265857*10**(-6)
    toUnitFactor FS = 24.188843265857*10**(-3)
    toUnitFactor AS = 24.188843265857
