module Maths.HilbertSpace.Norm where


class Normalisable a where
    {-# MINIMAL (norm | norm2), scale #-}
    norm  :: a -> Double
    norm2 :: a -> Double
    scale :: Double -> a -> a

    {-# INLINE norm  #-}
    {-# INLINE norm2 #-}
    norm  = sqrt  . norm2
    norm2 = (**2) . norm

normalise :: (Normalisable a) => a -> a
normalise x = (1/norm x)`scale`x
