module QState.Units.CrossSec
(   CrossSecUnit(..)
) where

import           QState.Units.Internal


data CrossSecUnit = AU | MBarn

instance Show CrossSecUnit where
    show MBarn = "Mbarn"
    show AU    = "au"

instance Read CrossSecUnit where
    readsPrec _ ('a':'u'            :str) = [(AU   ,str)]
    readsPrec _ ('M':'b':'a':'r':'n':str) = [(MBarn,str)]
    readsPrec _  str                      = error $ "cannot read cross section "
                                                  ++"unit "++str

instance Unit CrossSecUnit where
    toUnitFactor AU    = 1
    toUnitFactor MBarn = 28.0028521
