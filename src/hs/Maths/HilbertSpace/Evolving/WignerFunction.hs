module Maths.HilbertSpace.Evolving.WignerFunction
(   WignerFunction
,   getWignerFunctionFromDensityMatrix
,   wignerFunctionFromDensityMatrix
) where

import           Data.List                                 (genericLength)
import           Data.Maybe

import           Maths.HilbertSpace.Evolving
import           Maths.HilbertSpace.Distribution
import           Maths.HilbertSpace.Operator
import           Maths.HilbertSpace.Operator.DensityMatrix
import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar

import           QState
import           QState.Time
import           QState.Units


type WignerFunction = Evolving Ket


getWignerFunctionFromDensityMatrix :: DensityMatrix -> QState WignerFunction
getWignerFunctionFromDensityMatrix rho = wignerFunctionFromDensityMatrix rho
                                                                    <$>getTGrid

wignerFunctionFromDensityMatrix :: DensityMatrix -> [Double] -> WignerFunction
wignerFunctionFromDensityMatrix rho ts = evolving (Just Time) ts
                                       $ wignerFunctionFromDensityMatrixT rho

wignerFunctionFromDensityMatrixT :: DensityMatrix -> Double -> Ket
wignerFunctionFromDensityMatrixT rho t = ket ut es
                            $ map (wignerFunctionFromDensityMatrixTEi rho t) eis
    where
        ut  = unitType rho
        es  = basis    rho
        eis = [0..length (fromJust es)]

wignerFunctionFromDensityMatrixTEi :: DensityMatrix -> Double -> Int -> Scalar
wignerFunctionFromDensityMatrixTEi rho t eI = 2*fromReal dE*sum (map fftVal dCs)
    where
        dE = let es = fromJust $ basis rho in (last es-head es)/genericLength es
        bLen = length . fromJust $ basis rho

        dCs = let dIMax = (bLen-1-eI)`min`eI in [-dIMax..dIMax]

        fftVal :: Int -> Scalar
        fftVal dC = opElem rho (eI-dC) (eI+dC)*exp(-2*i(t*dE*fromIntegral dC))
