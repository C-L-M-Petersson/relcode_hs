{-# LANGUAGE FlexibleInstances #-}
module Maths.HilbertSpace.Operator.DensityMatrix where

import           Data.List
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
purity rho = assertReal $ trace (rho^2)

concurrence :: DensityMatrix -> Double
concurrence rho = sqrt( 2*(1-purity rho) )
