module QState.TwoPhoton.RPAE
(   mElementCorrection
,   mElementCorrectionKappa
) where

import           Data.Composition
import           Data.Maybe

import           Maths.HilbertSpace
import           Maths.QuantumNumbers
import           Maths.WignerSymbol

import           QState.Configure.Internal
import           QState.PertWave
import           QState.TwoPhoton.Internal


readFileColChannel :: (Num a,Read a) => FilePath -> QNum -> QNum -> QNum
                                                            -> CDict -> IO [a]
--readFileColChannel :: FilePath -> QNum -> QNum -> QNum
--                                                            -> CDict -> IO [Scalar]
readFileColChannel file kappa0 kappa2 jTot cDict
    | isNothing mCol = (`replicate`0) . length<$>fileLines file cDict
    | otherwise      = do putStrLn (show kappa0++" "++show kappa2++" "++show jTot++" "++show mCol)
                          (`filterBreakPoints`cDict)<$>readFileColIndex file (fromJust mCol) cDict
    where mCol = let mColJTot0
                        | kappa2==  kappa0                 = Just 0
                        | otherwise                        = Nothing
                     mColJTot2
                        | kappa2==  kappa0-2*signum kappa0 = Just 0
                        | kappa2== -kappa0+  signum kappa0 = Just 1
                        | kappa2==  kappa0                 = Just 2
                        | kappa2== -kappa0-  signum kappa0 = Just 3
                        | kappa2==  kappa0+2*signum kappa0 = Just 4
                        | otherwise                        = Nothing
                  in case jTot of 0 -> mColJTot0
                                  2 -> mColJTot2
                                  _ -> Nothing


mElementCorrectionPrimitive :: QNum -> QNum -> QNum -> QNum -> Int -> CDict
                                                                -> IO [Scalar]
mElementCorrectionPrimitive kappa0 n0 kappa2 jTot eFinalIndex cDict =
                    readFileColChannel mElemFilePath kappa0 kappa2 jTot cDict
    where mElemFilePath = "m_elem_2phRPAcorrection_eF"++show eFinalIndex++"_"
                        ++show kappa0++"_"++show (nthKappaElevel kappa0 n0)
                        ++"_Jtot"++show jTot

mElementCorrection :: QNum -> QNum -> QNum -> QNum -> QNum -> Int -> CDict
                                                                -> IO [Scalar]
mElementCorrection kappa0 n0 kappa2 mJ jTot eFinalIndex cDict = zipWith
                                                                ((*fact).:(*))
        <$>mElementCorrectionPrimitive kappa0 n0 kappa2 jTot eFinalIndex cDict
        <*>(map (subtractedPhaseFactor kappa2 (cDictReadOption "zEff" cDict))
                                    <$>energyFin kappa0 n0 eFinalIndex cDict)
--mElementCorrection kappa0 n0 kappa2 mJ jTot eFinalIndex cDict = map (*0)
--        -- <$>mElementCorrectionPrimitive kappa0 n0 kappa2 jTot eFinalIndex cDict
--        <$>(map (subtractedPhaseFactor kappa2 (cDictReadOption "zEff" cDict))
--                                    <$>energyFin kappa0 n0 eFinalIndex cDict)
    where
        j0 = jFromKappa kappa0
        j2 = jFromKappa kappa2
        fact = scalarFromQNum(2*jTot+1)
             * wigner3j   j2  jTot  j0
                        (-mJ)  0    mJ
             * wigner3j   1    1   jTot
                          0    0    0
             * (-1)**scalarFromQNum(2*j2+j0+jTot-mJ)

mElementCorrectionKappa :: QNum -> QNum -> QNum -> QNum -> Int -> CDict
                                                                -> IO [Scalar]
mElementCorrectionKappa kappa0 n0 kappa2 mJ eFinalIndex cDict
    | kappa0==kappa2 = zipWith (+)<$>elemJTot 0<*>elemJTot 2
    | otherwise      = elemJTot 2
    where elemJTot jTot = mElementCorrection kappa0 n0 kappa2 mJ jTot
                                                            eFinalIndex cDict
