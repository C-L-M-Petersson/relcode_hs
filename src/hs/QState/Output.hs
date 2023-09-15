module QState.Output
(   printQState
,   putStrQState
,   putStrLnQState

,   putStrQStateFile
,   printQStateFile
,   printQStateFileWithUnits
) where

import           Control.Lens ((??))
import           Control.Monad

import           Data.Composition

import           QState
import           QState.FilePath
import           QState.Units



printQState :: Show a => a -> QState()
printQState = liftIO . print

putStrQState :: String -> QState()
putStrQState = liftIO . putStr

putStrLnQState :: String -> QState()
putStrLnQState = liftIO . putStrLn


putStrQStateFile :: FilePath -> String -> QState()
putStrQStateFile fp x = createParentDir fp
                      >>join (liftIO.:writeFile<$>getCDictFilePath fp??x)

printQStateFile :: Show a => FilePath -> a -> QState()
printQStateFile fp = putStrQStateFile fp . show

printQStateFileWithUnits :: (HasUnit a,Show a) => FilePath -> a -> QState()
printQStateFileWithUnits fp x = toUnits x>>=printQStateFile fp
