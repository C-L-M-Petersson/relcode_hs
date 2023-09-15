module QState.OnePhoton.Internal
(   omegas
,   amps
,   phaseF
,   phaseG

,   matElem
) where

import           Data.List                   (transpose)

import           Maths.HilbertSpace
import           Maths.QuantumNumbers

import           QState.Configure
import           QState.FilePath.Internal
import           QState.HartreeFock.Internal
import           QState.Utility.Internal


fileLines :: FilePath -> QNum -> QNum -> CDict -> IO [String]
fileLines file kappa0 n0 cDict = lines<$>readFile fp
    where fp = pertDir kappa0 n0 cDict++"/"++file++".dat"

fileCol :: FilePath -> QNum -> QNum -> Int -> CDict -> IO [String]
fileCol file kappa0 n0 col cDict = map ((!!col) . words)
                                            <$>fileLines file kappa0 n0 cDict

readFileLines :: Read a => FilePath -> QNum -> QNum -> CDict -> IO [a]
readFileLines file kappa0 n0 cDict = map read<$>fileLines file kappa0 n0 cDict

readFileColIndex :: Read a => FilePath -> QNum -> QNum -> Int -> CDict -> IO [a]
readFileColIndex file kappa0 n0 col cDict = map read
                                            <$>fileCol file kappa0 n0 col cDict

readFileColKappa :: (Num a,Read a) => FilePath -> QNum -> QNum -> QNum -> CDict
                                                                    -> IO [a]
readFileColKappa file kappa0 n0 kappa1 cDict
    | col== -1  = (`replicate`0) . length<$>fileLines file kappa0 n0 cDict
    | otherwise = readFileColIndex file kappa0 n0 col cDict
    where col
            | kappa1==  kappa0-signum kappa0 =  1
            | kappa1== -kappa0               =  2
            | kappa1==  kappa0+signum kappa0 =  3
            | otherwise                      = -1


omegas :: QNum -> QNum -> CDict -> IO [Double]
omegas = readFileLines "omega"

eKins  :: QNum -> QNum -> CDict -> IO [Double]
eKins kappa0 n0 cDict = do
    hfE <- hfEnergy kappa0 n0 cDict
    os  <- omegas   kappa0 n0 cDict
    return $ map (+hfE) os

amps :: QNum -> QNum -> QNum -> CDict -> IO [Double]
amps = readFileColKappa "/amp_all"

phaseF :: QNum -> QNum -> QNum -> CDict -> IO [Double]
phaseF = readFileColKappa "/phaseF_all"

phaseG :: QNum -> QNum -> QNum -> CDict -> IO [Double]
phaseG = readFileColKappa "/phaseG_all"


matElem :: QNum -> QNum -> QNum -> CDict -> IO [Scalar]
matElem kappa0 n0 kappa1 cDict = map product . transpose
    <$>sequence [ map        fromReal <$>amps   kappa0 n0 kappa1 cDict
                , map (exp . fromImag)<$>phaseF kappa0 n0 kappa1 cDict
                , map (exp . fromImag  . negate . coulombPhase kappa1)
                                      <$>eKins kappa0 n0 cDict
                ]
