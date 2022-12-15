module QState.OnePhoton.Internal
(   omegas
,   amps
,   phaseF
,   phaseG

,   matElem
,   matElems
) where

import           Data.Composition
import           Data.List

import           Maths.HilbertSpace
import           Maths.QuantumNumbers

import           QState.Configure
import           QState.Energy.Internal
import           QState.FilePath.Internal
import           QState.HartreeFock.Internal
import           QState.Utility.Internal


fileLines :: String -> QNum -> QNum -> CDict -> IO [String]
fileLines file kappa n cDict = lines<$>readFile fp
    where fp = pertDir kappa n cDict++"/"++file

readFileLines :: Read a => String -> QNum -> QNum -> CDict -> IO [a]
readFileLines file kappa n cDict = map read<$>fileLines file kappa n cDict

readFileCol :: (Num a,Read a) => String -> QNum -> QNum -> QNum -> CDict
                                                                    -> IO [a]
readFileCol file kappa0 n kappa1 cDict
    | col== -1  = (`replicate`0) . length     <$>fileLines file kappa0 n cDict
    | otherwise = map (read . (!!col) . words)<$>fileLines file kappa0 n cDict
    where col
            | kappa1==  kappa0-signum kappa0 =  1
            | kappa1== -kappa0               =  2
            | kappa1==  kappa0+signum kappa0 =  3
            | otherwise                      = -1



omegas :: QNum -> QNum -> CDict -> IO [Double]
omegas = readFileLines "omega.dat"

eKins  :: QNum -> QNum -> CDict -> IO [Double]
eKins kappa0 n0 cDict = do
    hfE <- hfEnergy kappa0 n0 cDict
    os  <- omegas   kappa0 n0 cDict
    return $ map (+hfE) os

amps :: QNum -> QNum -> QNum -> CDict -> IO [Double]
amps = readFileCol "/amp_all.dat"

phaseF :: QNum -> QNum -> QNum -> CDict -> IO [Double]
phaseF = readFileCol "/phaseF_all.dat"

phaseG :: QNum -> QNum -> QNum -> CDict -> IO [Double]
phaseG = readFileCol "/phaseG_all.dat"



matElem :: QNum -> QNum -> QNum -> CDict -> IO [Scalar]
matElem kappa0 n0 kappa1 cDict = map product . transpose
    <$>sequence [ map        fromReal <$>amps   kappa0 n0 kappa1 cDict
                , map (exp . fromImag)<$>phaseF kappa0 n0 kappa1 cDict
                , map (exp . fromImag  . negate . coulombPhase kappa1)
                                      <$>eKins kappa0 n0 cDict
                ]

matElems :: [QNum] -> [QNum] -> [QNum] -> CDict -> IO [Scalar]
matElems  []               []      _       cDict = return $ repeat 0
matElems (kappa0:kappas0) (n0:ns0) kappas1 cDict = map product . transpose
    <$>sequence ( matElems kappas0 ns0 kappas1 cDict
                : map (flip (matElem kappa0 n0) cDict) kappas1 )
