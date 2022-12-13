module QState.OnePhoton.Internal
(   omegas
,   amps
,   phaseF
,   phaseG
) where

import           Maths.QuantumNumbers

import           QState.Internal.Configure
import           QState.Internal.Input.Directories


fileLines :: String -> QNum -> QNum -> CDict -> IO [String]
fileLines file kappa n cDict = lines<$>readFile fp
    where fp = pertDir kappa n cDict++"/"++file

readFileLines :: Read a => String -> QNum -> QNum -> CDict -> IO [a]
readFileLines file kappa n cDict = map read . lines<$>readFile fp
    where fp = pertDir kappa n cDict++"/"++file

readFileCol :: Read a => String -> QNum -> QNum -> QNum -> CDict -> IO [a]
readFileCol file kappa0 n kappa1 cDict = map (read . (!!col) . words)
                                            <$>fileLines file kappa0 n cDict
    where col
            | kappa1==  kappa0-signum kappa0 = 1
            | kappa1== -kappa0               = 2
            | kappa1==  kappa0+signum kappa0 = 3



omegas :: QNum -> QNum -> CDict -> IO [Double]
omegas = readFileLines "omega.dat"

amps :: QNum -> QNum -> QNum -> CDict -> IO [Double]
amps = readFileCol "/amp_all.dat"

phaseF :: QNum -> QNum -> QNum -> CDict -> IO [Double]
phaseF = readFileCol "/phaseF_all.dat"

phaseG :: QNum -> QNum -> QNum -> CDict -> IO [Double]
phaseG = readFileCol "/phaseG_all.dat"
