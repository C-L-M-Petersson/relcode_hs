module QState.Configure
(   CDict

,   getOption
,   getOptionSafe
,   getReadOption
,   getReadOptionSafe

,   withOptions
,   withOption
) where

import           Control.Monad.State

import           QState
import           QState.Internal
import           QState.Configure.Internal


getOption         ::           String -> QState String
getOption         = (<$>getCDict) . cDictOption

getOptionSafe     ::           String -> QState (Maybe String)
getOptionSafe     = (<$>getCDict) . cDictOptionSafe

getReadOption     :: Read a => String -> QState a
getReadOption     = (<$>getCDict) . cDictReadOption

getReadOptionSafe :: Read a => String -> QState (Maybe a)
getReadOptionSafe = (<$>getCDict) . cDictReadOptionSafe



withOptions :: [String] -> [String] -> QState a -> QState a
withOptions (k:ks) (v:vs) s = withOption k v (withOptions ks vs s)
withOptions  _      _     s = s

withOption :: String -> String -> QState a -> QState a
withOption k v s = do
    cDict <- getCDict
    modify (\s -> s{ cDict = cDictInsertOption k v cDict })
    x <- s
    modify (\s -> s{ cDict = cDict })

    return x
