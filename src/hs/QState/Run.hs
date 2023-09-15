module QState.Run
(   runQState
,   initialiseQState
) where

import           Control.Monad.State

import           QState
import           QState.Configure.Internal
import           QState.Internal

import           System.Directory


runQState :: QState a -> IO a
runQState s = initialiseQState>>=evalStateT s

initialiseQState :: IO System
initialiseQState = do
    cDict <- loadCDict

    let runDir = "runDir"`cDictOption`cDict
    runDirFiles <- listDirectory runDir

    omegasXUV <- readOmegasXUV runDir (((=="pert_").take 5)`filter`runDirFiles)

    return System
           { cDict_     = cDict
           , eUnit_     = cDictReadOption "energyUnits" cDict
           , tUnit_     = cDictReadOption "timeUnits"   cDict

           , twoPhoton_ = "second_photon"`elem`runDirFiles

           , omegasXUV_ = omegasXUV
           }
    where
        readOmegasXUV :: String -> [String] -> IO [Double]
        readOmegasXUV rD (fp:_) = let fp' = rD++fp++"/omega.dat"
                                   in map read . lines<$>readFile fp'
        readOmegasXUV _   _     = error "no pert directory in run directory"
