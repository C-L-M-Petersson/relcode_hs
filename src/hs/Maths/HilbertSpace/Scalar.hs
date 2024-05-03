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
,   scalarPhase

,   assertReal
,   assertImag

,   conj
) where

import           Data.Complex
import           Data.Composition
import           Data.List       (intercalate)
import           Data.List.Split
import           Data.List.Tools
import           Data.Maybe

import           QState
import           QState.Units


data Scalar = Scalar
              { scalarUnit_ :: Maybe UnitType
              , val_        :: Complex Double
              } deriving(Eq)

instance Floating Scalar where
        pi    = Scalar Nothing pi
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
    fromRational = Scalar Nothing . fromRational

instance HasUnit Scalar where
    unitType = scalarUnit_
    toUnits   (Scalar (Just ut) v) = let (r:+i) = v
                                      in (\f -> Scalar (Just ut) (f r:+f i))
                                                            . to  <$>getUnit ut
    toUnits    s                   = return s
    fromUnits (Scalar (Just ut) v) = let (r:+i) = v
                                      in (\f -> Scalar (Just ut) (f r:+f i))
                                                            . from<$>getUnit ut
    fromUnits  s                   = return s
    setUnit ut (Scalar _ v) = Scalar (Just ut) v

instance Num Scalar where
    negate      = smap negate
    (+)         = liftScalar (+)
    (*)         = liftScalar (*)
    fromInteger = Scalar Nothing . fromInteger
    abs         = smap abs
    signum      = smap signum

instance Ord Scalar where
    s`compare`s' = toReal s`compare`toReal s'
    s <=      s' = toReal s <=      toReal s'

instance Read Scalar where
    readsPrec _ str         = [(readHead,strTail)]
        where
            readHead = Scalar Nothing . uncurry (:+) . read
                     $ takeUntil (==')') str'
            strTail  = dropUntil (==')') str'
            str' = let replace from to = intercalate to . splitOn from
                    in replace "NaN" "0" str

instance Show Scalar where
    show (Scalar _ (r:+i_))
        | r ==0&&i_==0 = "0"
        | r ==0        =              show i_++"i"
        | i_==0        = show r
        | i_< 0        = show r     ++show i_++"i"
        | otherwise    = show r++"+"++show i_++"i"


smap :: (Complex Double -> Complex Double) -> Scalar -> Scalar
smap f (Scalar sU v) = Scalar sU (f v)

liftScalar :: (Complex Double -> Complex Double -> Complex Double) -> Scalar
                                                            -> Scalar -> Scalar
liftScalar f (Scalar Nothing v) (Scalar Nothing v') = Scalar Nothing (f v v')
liftScalar f (Scalar mUT     v) (Scalar Nothing v') = Scalar mUT     (f v v')
liftScalar f (Scalar Nothing v) (Scalar mUT'    v') = Scalar mUT'    (f v v')
liftScalar f (Scalar mUT     v) (Scalar mUT'    v')
    | mUT==mUT'                                     = Scalar mUT     (f v v')
    | otherwise                                     = error
        $ "can not add scalar of dimension "++show (fromJust mUT )
                                   ++" and "++show (fromJust mUT')


fromRealImag :: Double -> Double -> Scalar
fromRealImag = Scalar Nothing .: (:+)

fromReal :: Double -> Scalar
fromReal = Scalar Nothing . (:+0)

fromImag :: Double -> Scalar
fromImag = Scalar Nothing . (0:+)

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

scalarPhase :: Scalar -> Scalar
scalarPhase = fromReal . Data.Complex.phase . val_


assertReal :: Scalar -> Double
assertReal (Scalar _ (r:+0 )) = r
assertReal  s                 = error $ "scalar "++show s++" not real"

assertImag :: Scalar -> Double
assertImag (Scalar _ (0:+i_)) = i_
assertImag  s                 = error $ "scalar "++show s++" not imaginary"


conj :: Scalar -> Scalar
conj = smap conjugate
