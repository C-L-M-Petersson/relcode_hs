module Maths.HilbertSpace.Distribution where

import           Data.List
import           Data.Maybe


class Distributed a where
    {-# MINIMAL (norm | norm2), scale, basis, modifyBasis #-}
    norm  :: a -> Double
    norm2 :: a -> Double
    scale :: Double -> a -> a
    normalise :: a -> a

    basis            :: a -> Maybe [Double]
    modifyBasis      :: ([Double] -> [Double]) -> a -> a
    modifyBasisElems :: ( Double  ->  Double ) -> a -> a
    delta            :: a -> Double
    timesDelta       :: a -> a
    byDelta          :: a -> a

    {-# INLINE norm      #-}
    {-# INLINE norm2     #-}
    {-# INLINE normalise #-}
    norm  = sqrt  . norm2
    norm2 = (**2) . norm
    normalise x = (1/norm x)`scale`x


    {-# INLINE modifyBasisElems #-}
    {-# INLINE delta            #-}
    {-# INLINE timesDelta       #-}
    {-# INLINE byDelta          #-}
    modifyBasisElems = modifyBasis . map
    delta d
        | isJust (basis d) = let xs = fromJust $ basis d
                              in (last xs-head xs)/(genericLength xs-1)
        | otherwise        = 1
    timesDelta d =    delta d `scale`d
    byDelta    d = (1/delta d)`scale`d

shiftBasis :: Distributed a => Double -> a -> a
shiftBasis = modifyBasisElems . (+)
