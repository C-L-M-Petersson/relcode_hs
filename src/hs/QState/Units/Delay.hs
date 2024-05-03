{-# LANGUAGE FlexibleInstances #-}
module QState.Units.Delay
(   DelayUnit(..)
) where

import           QState.Units.Internal


data DelayUnit = Rad | AS Double

instance Show DelayUnit where
    show Rad    = "rad"
    show (AS _) = "as"

instance Read (Double -> DelayUnit) where
    readsPrec _ ('r':'a':'d':str) = [(const Rad,str)]
    readsPrec _ ('a':'s'    :str) = [(AS       ,str)]

--instance Read TimeUnit where
--    readsPrec _ ('a':'u':str) = [(AU,str)]
--    readsPrec _ (    's':str) = [(S ,str)]
--    readsPrec _ ('m':'s':str) = [(MS,str)]
--    readsPrec _ ('µ':'s':str) = [(US,str)]
--    readsPrec _ ('n':'s':str) = [(NS,str)]
--    readsPrec _ ('p':'s':str) = [(PS,str)]
--    readsPrec _ ('f':'s':str) = [(FS,str)]
--    readsPrec _ ('a':'s':str) = [(AS,str)]
--    readsPrec _  str          = error $ "cannot read time unit "++str

instance Unit DelayUnit where
    toUnitFactor  Rad       = 1
    toUnitFactor (AS omega) = 1/2/omega
