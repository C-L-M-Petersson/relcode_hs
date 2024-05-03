module Maths.HilbertSpace.Ket
(   Ket

,   ket
,   ketNullBasis
,   ithKet

,   ketBasisUnit
,   ketBasis
,   ketElems

,   kmap
,   kzip

,   (<|)
,   (.|>)
,   (<|>)
,   (|>|>)
) where

import           Data.List                       (intercalate)

import           GHC.Data.Maybe

import           Maths.HilbertSpace.Distribution
import           Maths.HilbertSpace.Scalar

import           QState
import           QState.Units


data Ket = Ket { ketBasisUnit_ :: Maybe UnitType
               , ketBasis_     :: Maybe [Double]
               , ketElems_     :: [Scalar]
               }

instance Distributed Ket where
    norm2      k = assertReal(k<|>k)
    scale        = (.|>) . fromReal
    basis        = ketBasis
    setBasis b k = k{ ketBasis_ = b }

instance HasUnit Ket where
    unitType = ketBasisUnit
    toUnits   (Ket mUT mB es) = Ket mUT<$>mB'<*>mapM toUnits   es
        where mB' = if isJust mUT&&isJust mB
                        then Just . (`map`fromJust mB) . to
                                                    <$>getUnit (fromJust mUT)
                        else return mB
    fromUnits (Ket mUT mB es) = Ket mUT<$>mB'<*>mapM fromUnits es
        where mB' = if isJust mUT&&isJust mB
                        then Just . (`map`fromJust mB) . from
                                                    <$>getUnit (fromJust mUT)
                        else return mB
    setUnit ut k              = k{ ketBasisUnit_ = Just ut }

instance Num Ket where
      negate      = kmap negate
      (+)         = liftKet2 (+)
      (*)         = liftKet2 (*)
      fromInteger = Ket Nothing Nothing . repeat . fromInteger
      abs         = kmap abs
      signum      = kmap signum

instance Show Ket where
    show (Ket _ Nothing  es) = ("|"++) . (++">") . intercalate ","
                             $ map show es
    show (Ket _ (Just b) es) = unlines $ zipWith showElem b es
        where showElem b_ e = unwords $ map show [b_,absVal e,phase e]


ket :: Maybe UnitType -> Maybe [Double] -> [Scalar] -> Ket
ket = Ket

ketNullBasis :: [Scalar] -> Ket
ketNullBasis = Ket Nothing Nothing

ithKet :: Int -> Ket
ithKet ind = Ket Nothing Nothing (replicate ind 0++[1]++repeat 0)


ketBasisUnit :: Ket -> Maybe UnitType
ketBasisUnit = ketBasisUnit_

ketBasis :: Ket -> Maybe [Double]
ketBasis = ketBasis_

ketElems :: Ket -> [Scalar]
ketElems = ketElems_


kmap :: (Scalar -> Scalar) -> Ket -> Ket
kmap f k = k{ ketElems_ = f`map`ketElems k }

kzip :: (Scalar -> Scalar -> Scalar) -> Ket -> Ket -> Ket
kzip = liftKet2

liftKet2 :: (Scalar -> Scalar -> Scalar) -> Ket -> Ket -> Ket
liftKet2 f (Ket uT mB es) (Ket uT' mB' es') = Ket (uT`firstJust`uT')
                                                  (mB`firstJust`mB')
                                                  (zipWith f es es')


(<|) :: Ket -> Ket
(<|) = kmap conj

(.|>) :: Scalar -> Ket -> Ket
(.|>) = kmap . (*)

(<|>) :: Ket -> Ket -> Scalar
k<|>k' = (<|)k|>|>k'

(|>|>) :: Ket -> Ket -> Scalar
k|>|>k'
    | basis k==basis k' = (fromReal(delta k)*) . sum . ketElems $ k*k'
    | otherwise         = error $ "taking scalar product of kets with different"
                                ++" bases"
