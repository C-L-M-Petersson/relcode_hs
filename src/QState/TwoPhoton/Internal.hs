module QState.TwoPhoton.Internal
(   energyRPA
,   energyFin

,   mElement
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


fileLines :: FilePath -> CDict -> IO [String]
fileLines file cDict = lines<$>readFile fp
    where fp = secondPhotonDir cDict++"/"++file++".dat"

fileCol :: FilePath -> Int -> CDict -> IO [String]
fileCol file col cDict = map ((!!col) . words)<$>fileLines file cDict

readFileLines :: Read a => FilePath -> CDict -> IO [a]
readFileLines file cDict = map read<$>fileLines file cDict

readFileColIndex :: (Num a,Read a) => FilePath -> Int -> CDict -> IO [a]
readFileColIndex file col cDict = map read<$>fileCol file col cDict

readFileColKappa :: (Num a,Read a) => FilePath -> QNum -> QNum -> QNum -> CDict
                                                                    -> IO [a]
readFileColKappa file kappa0 kappa1 kappa2 cDict
    | col== -1  = (`replicate`0) . length<$>fileLines file cDict
    | otherwise = readFileColIndex file col cDict
    where
        col = colOuter+colInner
        colOuter
            | kappa1==  kappa0-signum kappa0 =  0
            | kappa1== -kappa0               =  3
            | kappa1==  kappa0+signum kappa0 =  6
            | otherwise                      = -1
        colInner
            | kappa2==  kappa1-signum kappa0 =  0
            | kappa2== -kappa1               =  1
            | kappa2==  kappa1+signum kappa0 =  2
            | otherwise                      = -1



energyRPA :: QNum -> QNum -> CDict -> IO [Double]
energyRPA kappa0 n0 = readFileLines
    $ "energy_rpa_"++show kappa0++"_"++show (nthKappaElevel kappa0 n0)

energyFin :: QNum -> QNum -> Int -> CDict -> IO [Double]
energyFin kappa0 n0 = readFileColIndex
    ("energy_fin_"++show kappa0++"_"++show (nthKappaElevel kappa0 n0))
    . subtract 1



mElement :: QNum -> QNum -> QNum -> QNum -> Int -> CDict -> IO [Scalar]
mElement kappa0 n0 kappa1 kappa2 eFinalIndex = (filterLines<$>).readFileColKappa
    ("m_elements_eF"++show eFinalIndex++"_"++show kappa0
                                      ++"_"++show (nthKappaElevel kappa0 n0))
        kappa0 kappa1 kappa2
    where
        filterLines :: [a] -> [a]
        filterLines (x:xs) = everyFifth (x:x:xs)
            where everyFifth xs = case drop 4 xs of x:xs' -> x : everyFifth xs'
                                                    []    -> []
