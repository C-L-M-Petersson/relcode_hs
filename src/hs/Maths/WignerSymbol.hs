module Maths.WignerSymbol
(   wigner3j
,   clebschGordan
) where


import           Maths.HilbertSpace.Scalar
import           Maths.QuantumNumbers.Internal

import qualified WignerSymbols


sixValuesSymbol :: ((Int,   Int,    Int,    Int,    Int,    Int) -> Double)
                 -> QNum -> QNum -> QNum -> QNum -> QNum -> QNum -> Scalar
sixValuesSymbol symbol qN00 qN01 qN02 qN10 qN11 qN12 = fromReal $ symbol
                                ( intFromQNum (2*qN00), intFromQNum (2*qN10)
                                , intFromQNum (2*qN01), intFromQNum (2*qN11)
                                , intFromQNum (2*qN02), intFromQNum (2*qN12) )


wigner3j :: QNum -> QNum -> QNum -> QNum -> QNum -> QNum -> Scalar
wigner3j j1 j2 j3
         m1 m2 m3 = sixValuesSymbol WignerSymbols.wigner3j j1 j2 j3
                                                           m1 m2 m3

clebschGordan :: QNum -> QNum -> QNum -> QNum -> QNum -> QNum -> Scalar
clebschGordan j1 m1 j2 m2
                    j  m  = sixValuesSymbol WignerSymbols.clebschGordan j1 j2 j
                                                                        m1 m2 m
