{-# LANGUAGE FlexibleInstances #-}
module Maths.HilbertSpace.Operator.DensityMatrix
(   DensityMatrix

,   fromState
,   fromStates

,   purity
,   concurrence
) where

import           Data.Tuple.Extra

import           Maths.HilbertSpace.Distribution
import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Operator
import           Maths.HilbertSpace.Scalar


type DensityMatrix = Operator


fromState :: Ket -> DensityMatrix
fromState k = normalise(k|><|k)

fromStates :: [Ket] -> DensityMatrix
fromStates = normalise . sum . map (uncurry (|><|) . dupe)


purity :: DensityMatrix -> Double
purity rho = assertReal $ trace (rho^(2::Int))

concurrence :: DensityMatrix -> Double
concurrence rho = sqrt( 2*(1-purity rho) )
