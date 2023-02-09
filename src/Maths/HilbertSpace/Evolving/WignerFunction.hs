module Maths.HilbertSpace.Evolving.WignerFunction where
-- (   WignerFunction
-- ,   getWignerFunctionFromDensityMatrix
-- ,   wignerFunctionFromDensityMatrix
-- ) where

import           Data.List
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
import           QState.Units.Internal


type WignerFunction = Evolving Ket


getWignerFunctionFromDensityMatrix :: DensityMatrix -> QState WignerFunction
getWignerFunctionFromDensityMatrix rho = wignerFunctionFromDensityMatrix rho
                                                                    <$>getTGrid

wignerFunctionFromDensityMatrix :: DensityMatrix -> [Double] -> WignerFunction
wignerFunctionFromDensityMatrix rho ts = evolving (Just Time) ts
                                       $ wignerFunctionFromDensityMatrixT rho

wignerFunctionFromDensityMatrixT :: DensityMatrix -> Double -> Ket
wignerFunctionFromDensityMatrixT rho t = Ket ut es
                            $ map (wignerFunctionFromDensityMatrixTEi rho t) eis
    where
        ut  = unitType rho
        es  = basis    rho
        eis = [0..length (fromJust es)]

wignerFunctionFromDensityMatrixTEi :: DensityMatrix -> Double -> Int -> Scalar
wignerFunctionFromDensityMatrixTEi rho t eI = 2*fromReal dE*opElem rho eI eI
                                            + valsRec (-dE) (eI-1) (eI+1) True
                                            + valsRec   dE  (eI+1) (eI-1) False
    where
        dE = let es = fromJust $ basis rho
              in (last es-head es)/genericLength es
        bLen = length . fromJust $ basis rho
        halfBLen = bLen`div`2

        valsRec :: Double -> Int -> Int -> Bool -> Scalar
        valsRec deltaE r c posDeltaE
            | r<0 || r>=bLen = 0
            | c<0 || c>=bLen = 0
            | posDeltaE      = val+valsRec (deltaE+dE) (r-1) (c+1) posDeltaE
            | otherwise      = val+valsRec (deltaE-dE) (r+1) (c-1) posDeltaE
            where val = 2*fromReal dE*opElem rho r c*exp(-2*i(t*deltaE))
