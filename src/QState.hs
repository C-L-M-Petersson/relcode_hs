module QState
(   QState

,   liftIO

,   getCDict
,   withCDict
,   withCDictM

,   getOption
,   getOptionSafe
,   getReadOption
,   getReadOptionSafe
) where

import           Control.Monad.State

import           Data.Composition

import           Maths.QuantumNumbers

import           QState.Internal
import           QState.Internal.Configure


type QState a = StateT System IO a

getCDict :: QState CDict
getCDict = gets cDict

withCDict  :: (CDict -> a) -> QState a
withCDict  = (<$>getCDict)

withCDictM :: (CDict -> IO a) -> QState a
withCDictM x = liftIO . x=<<getCDict



getOption         ::           String -> QState String
getOption         = (<$>getCDict) . cDictOption

getOptionSafe     ::           String -> QState (Maybe String)
getOptionSafe     = (<$>getCDict) . cDictOptionSafe

getReadOption     :: Read a => String -> QState a
getReadOption     = (<$>getCDict) . cDictReadOption

getReadOptionSafe :: Read a => String -> QState (Maybe a)
getReadOptionSafe = (<$>getCDict) . cDictReadOptionSafe



getOmegasXUV :: QState [Double]
getOmegasXUV = gets omegasXUV
