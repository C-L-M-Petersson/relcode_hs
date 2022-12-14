{-# LANGUAGE FlexibleInstances #-}
module Maths.HilbertSpace.Scalar
(   Scalar

,   fromReal
,   fromImag
,   i

,   toReal
,   toImag
,   absVal
,   Maths.HilbertSpace.Scalar.phase

,   assertReal
,   assertImag

,   conj
) where

import           Data.Complex

import           Maths.HilbertSpace.Norm


newtype Scalar = Scalar { val :: Complex Double }

instance Eq Scalar where
    s==s' = val s==val s'

instance Floating Scalar where
        pi    = Scalar pi
        exp   = smap exp
        log   = smap log
        sin   = smap sin
        cos   = smap cos
        asin  = smap asin
        acos  = smap acos
        atan  = smap atan
        sinh  = smap sinh
        cosh  = smap cosh
        asinh = smap asinh
        acosh = smap acosh
        atanh = smap atanh



instance Fractional Scalar where
    (/)          = liftScalar (/)
    fromRational = Scalar . fromRational

instance Normalisable Scalar where
    norm2   s = toReal(conj s*s)
    scale d s = fromReal d*s

instance Num Scalar where
    negate      = smap negate
    (+)         = liftScalar (+)
    (*)         = liftScalar (*)
    fromInteger = Scalar . fromInteger
    abs         = smap abs
    signum      = smap signum

instance Ord Scalar where
    s`compare`s' = toReal s`compare`toReal s'
    s <=      s' = toReal s <=      toReal s'

instance Show Scalar where
    show (Scalar (r:+i))
        | r==0&&i==0 = "0"
        | r==0       = show r
        | i==0       = show r++"i"
        | i< 0       = show r++show i++"i"
        | otherwise  = show r++"+"++show i



smap :: (Complex Double -> Complex Double) -> Scalar -> Scalar
smap f = Scalar . f . val

liftScalar :: (Complex Double -> Complex Double -> Complex Double) -> Scalar -> Scalar -> Scalar
liftScalar f (Scalar v) (Scalar v') = Scalar (f v v')



fromReal :: Double -> Scalar
fromReal = Scalar . (:+0)

fromImag :: Double -> Scalar
fromImag = Scalar . (0:+)

i :: Double -> Scalar
i = fromImag



toReal :: Scalar -> Double
toReal = realPart . val

toImag :: Scalar -> Double
toImag = imagPart . val

absVal :: Scalar -> Double
absVal = magnitude . val

phase :: Scalar -> Double
phase = Data.Complex.phase . val



assertReal :: Scalar -> Double
assertReal (Scalar (r:+0)) = r
assertReal  s              = error $ "scalar "++show s++" not real"

assertImag :: Scalar -> Double
assertImag (Scalar (0:+i)) = i
assertImag  s              = error $ "scalar "++show s++" not imaginary"



conj :: Scalar -> Scalar
conj = smap conjugate
