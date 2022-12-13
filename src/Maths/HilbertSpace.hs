{-# LANGUAGE FlexibleInstances #-}
module Maths.HilbertSpace where

import           Control.Applicative hiding ((<|>))

import           Data.Complex
import           Data.List



class Normalisable a where
    {-# MINIMAL (norm | norm2), scale #-}
    norm  :: a -> Scalar
    norm2 :: a -> Scalar
    scale :: Scalar -> a -> a

    {-# INLINE norm  #-}
    {-# INLINE norm2 #-}
    norm  = sqrt  . norm2
    norm2 = (**2) . norm

normalise :: (Normalisable a) => a -> a
normalise x = (1/norm x)`scale`x



newtype Vec a = Vec { elems :: [a] }

instance Applicative Vec where
    pure          = Vec . repeat -- Dangerous!?
    Vec f<*>Vec x = Vec $ zipWith ($) f x

instance Functor Vec where
    fmap f k = k{ elems = map f (elems k) }
    (<$)     = fmap . const

instance Num a => Num (Vec a) where
      negate        = fmap negate
      (+)           = liftA2 (+)
      (*)           = liftA2 (*)
      fromInteger n = Vec (fromInteger n`replicate`0++[1])-- ++repeat 0)
      abs           = fmap abs
      signum        = fmap signum



type Scalar   = Complex Double

instance Ord Scalar where
    s`compare`s' = realPart s`compare`realPart s'
    s <=      s' = realPart s <=      realPart s'

type Ket      = Vec Scalar

instance Show Ket where
    show = ("|"++) . (++">") . intercalate "," . map show . elems

type Operator = Vec Ket



(<|) :: Ket -> Ket
(<|) = fmap conjugate

(.|>) :: Scalar -> Ket -> Ket
(.|>) = fmap . (*)

(<|>) :: Ket -> Ket -> Scalar
k<|>k' = sum . elems $ (<|)k*k'

(|><|>) :: Operator -> Ket -> Ket
o|><|>k  = sum $ zipWith (.|>) (elems k) (elems o)

(|><|) :: Ket -> Ket -> Operator
k|><|k' = (.|>k)`fmap`(<|)k'

(|><|><|) :: Operator -> Operator -> Operator
o|><|><|o' = (o|><|>)`fmap`o'



trace :: Operator -> Scalar
trace = sum . traceRec 0 . map elems . elems
    where
        traceRec :: Int -> [[Scalar]] -> [Scalar]
        traceRec i (xs:xss) = xs!!i : traceRec (i+1) xss
        traceRec _  _       = []
