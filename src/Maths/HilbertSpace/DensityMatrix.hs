{-# LANGUAGE FlexibleInstances #-}
module Maths.HilbertSpace.DensityMatrix where

import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Norm
import           Maths.HilbertSpace.Operator
import           Maths.HilbertSpace.Scalar


type DensityMatrix = Operator

instance Normalisable DensityMatrix where
    norm  = assertReal . trace
    scale = opmap . (.|>) . fromReal



fromState :: Ket -> DensityMatrix
fromState k = k|><|k

fromStates :: [Ket] -> DensityMatrix
fromStates = sum . map fromState
