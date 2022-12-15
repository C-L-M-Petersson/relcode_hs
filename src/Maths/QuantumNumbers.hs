module Maths.QuantumNumbers
(   QNum

,   intFromQNum
,   doubleFromQNum

,   kappaFromJL
,   lFromKappa
,   jFromKappa
,   nthKappaElevel

,   reachableKappas
) where

import           Data.Composition

import           Maths.QuantumNumbers.Error
import           Maths.QuantumNumbers.Internal (QNum, intFromQNum,
                                                doubleFromQNum)
import qualified Maths.QuantumNumbers.Internal as I


kappaFromJL :: QNum -> QNum -> QNum
kappaFromJL j l = checkJL j l $ I.kappaFromJL j l

lFromKappa :: QNum -> QNum
lFromKappa kappa = checkKappa kappa $ I.lFromKappa kappa

jFromKappa :: QNum -> QNum
jFromKappa kappa = checkKappa kappa $ abs kappa-1/2

nthKappaElevel :: QNum -> QNum -> QNum
nthKappaElevel kappa n = checkKappaN kappa n $ n-I.lFromKappa kappa



reachableKappas :: QNum -> [QNum]
reachableKappas kappa = checkKappa kappa $ I.reachableKappas kappa
