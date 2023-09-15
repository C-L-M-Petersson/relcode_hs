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
import           Data.List       (intercalate)
import           Data.List.Split
import           Data.List.Tools


newtype Scalar = Scalar { val_ :: Complex Double }

instance Eq Scalar where
    s==s' = val_ s==val_ s'

instance Floating Scalar where
        pi    = Scalar pi
        exp   = smap exp
        log   = smap log
        sqrt  = smap sqrt
        (**)  = liftScalar (**)
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

instance Read Scalar where
    readsPrec _ str         = [(readHead,strTail)]
        where
            readHead = Scalar . uncurry (:+) . read
                     $ takeUntil (==')') str'
            strTail  = dropUntil (==')') str'
            str' = let replace from to = intercalate to . splitOn from
                    in replace "NaN" "0" str

instance Show Scalar where
    show (Scalar (r:+i_))
        | r ==0&&i_==0 = "0"
        | r ==0        =              show i_++"i"
        | i_==0        = show r
        | i_< 0        = show r     ++show i_++"i"
        | otherwise    = show r++"+"++show i_++"i"



smap :: (Complex Double -> Complex Double) -> Scalar -> Scalar
smap f = Scalar . f . val_

liftScalar :: (Complex Double -> Complex Double -> Complex Double) -> Scalar
                                                            -> Scalar -> Scalar
liftScalar f (Scalar v) (Scalar v') = Scalar (f v v')



fromReal :: Double -> Scalar
fromReal = Scalar . (:+0)

fromImag :: Double -> Scalar
fromImag = Scalar . (0:+)

i :: Double -> Scalar
i = fromImag



toReal :: Scalar -> Double
toReal = realPart . val_

toImag :: Scalar -> Double
toImag = imagPart . val_

absVal :: Scalar -> Double
absVal = magnitude . val_

phase :: Scalar -> Double
phase = Data.Complex.phase . val_



assertReal :: Scalar -> Double
assertReal (Scalar (r:+0 )) = r
assertReal  s               = error $ "scalar "++show s++" not real"

assertImag :: Scalar -> Double
assertImag (Scalar (0:+i_)) = i_
assertImag  s               = error $ "scalar "++show s++" not imaginary"



conj :: Scalar -> Scalar
conj = smap conjugate
