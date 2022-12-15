module QState.Configure
(   CDict

,   getOption
,   getOptionSafe
,   getReadOption
,   getReadOptionSafe
) where

import           QState
import           QState.Configure.Internal


getOption         ::           String -> QState String
getOption         = (<$>getCDict) . cDictOption

getOptionSafe     ::           String -> QState (Maybe String)
getOptionSafe     = (<$>getCDict) . cDictOptionSafe

getReadOption     :: Read a => String -> QState a
getReadOption     = (<$>getCDict) . cDictReadOption

getReadOptionSafe :: Read a => String -> QState (Maybe a)
getReadOptionSafe = (<$>getCDict) . cDictReadOptionSafe
