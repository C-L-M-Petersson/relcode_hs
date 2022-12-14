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

,   getEnergyUnit

,   getOmegasXUV
) where

import           Control.Monad.State

import           Data.Composition

import           Maths.QuantumNumbers

import           QState.Configure
import           QState.Internal
import           QState.Units.Internal


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



getEnergyUnit :: QState EnergyUnit
getEnergyUnit = gets eUnit



getOmegasXUV :: QState [Double]
getOmegasXUV = gets omegasXUV
