module Maths.HilbertSpace.Ket where

import           Control.Applicative hiding ((<|>))

import           Data.List
import           Data.Maybe

import           Maths.HilbertSpace.Scalar

import           Safe


data Ket = Ket { basis    :: Maybe [Double]
               , ketElems :: [Scalar]
               }

instance Num Ket where
      negate      = kmap negate
      (+)         = liftKet2 (+)
      (*)         = liftKet2 (*)
      fromInteger = Ket Nothing . repeat . fromInteger
      abs         = kmap abs
      signum      = kmap signum

instance Show Ket where
    show (Ket Nothing  es) = ("|"++) . (++">") . intercalate "," $ map show es
    show (Ket (Just b) es) = unlines $ zipWith showElem b es
        where showElem b e = unwords $ map show [b,absVal e,phase e]



ket :: [Scalar] -> Ket
ket = Ket Nothing

ithKet :: Int -> Ket
ithKet i = Ket Nothing (replicate i 0++[1]++repeat 0)



kmap :: (Scalar -> Scalar) -> Ket -> Ket
kmap f k = k{ ketElems = f`map`ketElems k }

liftKet2 :: (Scalar -> Scalar -> Scalar) -> Ket -> Ket -> Ket
liftKet2 f (Ket mb es) (Ket mb' es') = Ket (headMay $ catMaybes [mb,mb'])
                                           (zipWith f es es')



(<|) :: Ket -> Ket
(<|) = kmap conj

(.|>) :: Scalar -> Ket -> Ket
(.|>) = kmap . (*)

(<|>) :: Ket -> Ket -> Scalar
k<|>k' = sum . ketElems $ (<|)k*k'
