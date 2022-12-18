module Maths.HilbertSpace.Distribution where

import           Data.List
import           Data.Maybe


class Distributed a where
    {-# MINIMAL (norm | norm2), scale, basis, (setBasis | modifyBasis) #-}
    norm  :: a -> Double
    norm2 :: a -> Double
    scale :: Double -> a -> a
    normalise :: a -> a

    basis            :: a -> Maybe [Double]
    setBasis         :: Maybe [Double] -> a -> a
    modifyBasis      :: ([Double] -> Maybe [Double]) -> a -> a
    modifyBasisElems :: (Double -> Double) -> a -> a
    delta            :: a -> Double
    timesDelta       :: a -> a
    byDelta          :: a -> a

    {-# INLINE norm      #-}
    {-# INLINE norm2     #-}
    {-# INLINE normalise #-}
    norm  = sqrt  . norm2
    norm2 = (**2) . norm
    normalise x = (1/norm x)`scale`x

    {-# INLINE setBasis #-}
    {-# INLINE modifyBasis #-}
    setBasis        = modifyBasis . const
    modifyBasis f d = setBasis (basis d>>=f) d

    {-# INLINE modifyBasisElems #-}
    {-# INLINE delta            #-}
    {-# INLINE timesDelta       #-}
    {-# INLINE byDelta          #-}
    modifyBasisElems f = modifyBasis (Just . map f)
    delta d
        | isJust (basis d) = let xs = fromJust $ basis d
                              in (last xs-head xs)/(genericLength xs-1)
        | otherwise        = 1
    timesDelta d =    delta d `scale`d
    byDelta    d = (1/delta d)`scale`d

scaleBasis :: Distributed a => Double -> a -> a
scaleBasis = modifyBasisElems . (*)

shiftBasis :: Distributed a => Double -> a -> a
shiftBasis = modifyBasisElems . (+)



basisHead :: Distributed a => a -> Maybe Double
basisHead d = head<$>basis d

basisTail :: Distributed a => a -> Maybe Double
basisTail d = last<$>basis d
