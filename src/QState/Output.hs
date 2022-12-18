module QState.Output where
import QState.Units.Internal

import           Control.Lens ((??))
import           Control.Monad

import           Data.Composition

import           Maths.HilbertSpace.Distribution

import           QState
import           QState.FilePath



printQState :: Show a => a -> QState()
printQState = liftIO . print

putStrQState :: String -> QState()
putStrQState = liftIO . putStr

putStrLnQState :: String -> QState()
putStrLnQState = liftIO . putStrLn


putStrQStateFile :: String -> String -> QState()
putStrQStateFile str x = createParentDir str
                       >>join (liftIO.:writeFile<$>getCDictFilePath str??x)

printQStateFile :: Show a => String -> a -> QState()
printQStateFile str = putStrQStateFile str . show

printEnergyDistributionQStateFile :: (Distributed a,Show a) => String -> a
                                                                    -> QState()
printEnergyDistributionQStateFile str k = printQStateFile str=<<
    (`modifyBasisElems`k) . to<$>getEnergyUnit
