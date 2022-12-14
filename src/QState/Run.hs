module QState.Run where

import           System.Directory
import           Control.Monad.State

import           QState
import           QState.Configure
import           QState.Configure.Internal
import           QState.Internal


runQState :: QState a -> IO a
runQState s = initialiseQState>>=evalStateT s

initialiseQState :: IO System
initialiseQState = do
    cDict <- loadCDict

    let runDir = "runDir"`cDictOption`cDict
    runDirFiles <- listDirectory runDir

    omegasXUV <- getOmegasXUV runDir (((=="pert_") . take 5)`filter`runDirFiles)

    return System
           { cDict     = cDict
           , eUnit     = cDictReadOption "units" cDict

           , twoPhoton = "second_photon"`elem`runDirFiles

           , omegasXUV = omegasXUV
           }
    where
        getOmegasXUV :: String -> [String] -> IO [Double]
        getOmegasXUV rD (fp:_) = let fp' = rD++fp++"/omega.dat"
                                     in map read . lines<$>readFile fp'
        getOmegasXUV _   _     = error "no pert directory in run directory"
