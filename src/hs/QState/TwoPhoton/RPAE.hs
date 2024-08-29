module QState.TwoPhoton.RPAE
(   mElement2phCorrection
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
readFileColChannel file kappa0 kappa2 jTot cDict
    | isNothing mCol = (`replicate`0) . length<$>fileLines file cDict
    | otherwise      = (`filterBreakPoints`cDict)<$>readFileColIndex file (fromJust mCol) cDict
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


mElement2phCorrectionJTotPrimitive :: QNum -> QNum -> QNum -> QNum -> Int
                                                        -> CDict -> IO [Scalar]
mElement2phCorrectionJTotPrimitive kappa0 n0 kappa2 jTot eFinalIndex cDict =
                    readFileColChannel mElemFile kappa0 kappa2 jTot cDict
    where mElemFile = "m_elem_2phRPAcorrection_eF"++show eFinalIndex++"_"
                    ++show kappa0++"_"++show (nthKappaElevel kappa0 n0)
                    ++"_Jtot"++show jTot

mElement2phCorrectionJTot :: QNum -> QNum -> QNum -> QNum -> QNum -> Int
                                                        -> CDict -> IO [Scalar]
mElement2phCorrectionJTot kappa0 n0 kappa2 mJ jTot eFinalIndex cDict = zipWith
                                                                ((*fact).:(*))
        <$>mElement2phCorrectionJTotPrimitive kappa0 n0 kappa2 jTot eFinalIndex
                                                                        cDict
        <*>(map (subtractedPhaseFactor kappa2 (cDictReadOption "zEff" cDict))
                                    <$>energyFin kappa0 n0 eFinalIndex cDict)
    where
        j0 = jFromKappa kappa0
        j2 = jFromKappa kappa2
        fact = scalarFromQNum(2*jTot+1)
             * wigner3j   j2  jTot  j0
                        (-mJ)  0    mJ
             * wigner3j   1    1   jTot
                          0    0    0
             * (-1)**scalarFromQNum(2*j2+j0+jTot-mJ)

mElement2phCorrection :: QNum -> QNum -> QNum -> QNum -> Int -> CDict
                                                                -> IO [Scalar]
mElement2phCorrection kappa0 n0 kappa2 mJ eFinalIndex cDict
    | kappa0==kappa2 = zipWith (+)<$>elemJTot 0<*>elemJTot 2
    | otherwise      = elemJTot 2
    where elemJTot jTot = mElement2phCorrectionJTot kappa0 n0 kappa2 mJ jTot
                                                            eFinalIndex cDict
