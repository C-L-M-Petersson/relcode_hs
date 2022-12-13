{-# LANGUAGE FlexibleInstances #-}
module Maths.HilbertSpace.DensityMatrix where

import           Maths.HilbertSpace



type DensityMatrix = Operator

instance Normalisable DensityMatrix where
    norm  = trace
    scale = fmap . (.|>)




fromState :: Ket -> DensityMatrix
fromState k = k|><|k

fromStates :: [Ket] -> DensityMatrix
fromStates = sum . map fromState
