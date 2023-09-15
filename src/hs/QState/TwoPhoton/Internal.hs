-- {-# #-}jkjjkjg
module QState.TwoPhoton.Internal
(   energyRPA
,   energyFin

,   mElement
) where

import           Data.Maybe

import           Maths.HilbertSpace
import           Maths.QuantumNumbers
import           Maths.WignerSymbol

import           QState.Configure
import           QState.FilePath.Internal


fileLines :: FilePath -> CDict -> IO [String]
fileLines file cDict = lines<$>readFile fp
    where fp = secondPhotonDir cDict++"/"++file++".dat"

fileCol :: FilePath -> Int -> CDict -> IO [String]
fileCol file col cDict = map ((!!col) . words)<$>fileLines file cDict

readFileLines :: Read a => FilePath -> CDict -> IO [a]
readFileLines file cDict = map read<$>fileLines file cDict

readFileColIndex :: Read a => FilePath -> Int -> CDict -> IO [a]
readFileColIndex file col cDict = map read<$>fileCol file col cDict

readFileColKappa :: (Num a,Read a) => FilePath -> QNum -> QNum -> QNum -> CDict
                                                                    -> IO [a]
readFileColKappa file kappa0 kappa1 kappa2 cDict
    | isNothing mCol = (`replicate`0) . length<$>fileLines file cDict
    | otherwise      = readFileColIndex file (fromJust mCol) cDict
    where
        mCol = (+)<$>colOuter<*>colInner
        colOuter
            | kappa1==  kappa0-signum kappa0 = Just 0
            | kappa1== -kappa0               = Just 3
            | kappa1==  kappa0+signum kappa0 = Just 6
            | otherwise                      = Nothing
        colInner
            | kappa2==  kappa1-signum kappa1 = Just 0
            | kappa2== -kappa1               = Just 1
            | kappa2==  kappa1+signum kappa1 = Just 2
            | otherwise                      = Nothing



energyRPA :: QNum -> QNum -> CDict -> IO [Double]
energyRPA kappa0 n0 = readFileLines
    $ "energy_rpa_"++show kappa0++"_"++show (nthKappaElevel kappa0 n0)

energyFin :: QNum -> QNum -> Int -> CDict -> IO [Double]
energyFin kappa0 n0 = readFileColIndex
    ("energy_fin_"++show kappa0++"_"++show (nthKappaElevel kappa0 n0))
    . subtract 1



mElementPrimitive :: QNum -> QNum -> QNum -> QNum -> Int -> CDict -> IO [Scalar]
mElementPrimitive kappa0 n0 kappa1 kappa2 eFinalIndex = (filterLines<$>)
                        . readFileColKappa mElemFilePath kappa0 kappa1 kappa2
    where
        filterLines :: [a] -> [a]
        filterLines    []  = error "file empty"
        filterLines (x:xs) = everyFifth (x:x:xs)
            where everyFifth x_s = case drop 4 x_s of x_:x_s' -> x_ : everyFifth x_s'
                                                      []      -> []

        mElemFilePath = "m_elements_eF"++show eFinalIndex++"_"++show kappa0
                                  ++"_"++show (nthKappaElevel kappa0 n0)

mElement :: QNum -> QNum -> QNum -> QNum -> QNum -> Int -> CDict -> IO [Scalar]
mElement kappa0 n0 kappa1 kappa2 mJ eFinalIndex cDict = map (*fact)
        <$>mElementPrimitive kappa0 n0 kappa1 kappa2 eFinalIndex cDict
    where
        j0 = jFromKappa kappa0
        j1 = jFromKappa kappa1
        j2 = jFromKappa kappa2
        fact = wigner3j j2 1 j1 (-mJ) 0 mJ * wigner3j j1 1 j0 (-mJ) 0 mJ
             * (-1)**scalarFromQNum(j2+j1-2*mJ)
