module Maths.HilbertSpace.Operator where

import           Data.Composition
import           Data.List
import           Data.Maybe

import           Maths.HilbertSpace.Distribution
import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar

import           Safe


data Operator = Operator { opBasisOuter :: Maybe [Double]
                         , opCols       :: [Ket]
                         }

instance Distributed Operator where
    norm          o = assertReal (trace o)
    scale           = (.|><|) . fromReal
    basis           = opBasisOuter
    modifyBasis f o = o{ opBasisOuter = f<$>opBasisOuter o }

instance Num Operator where
      negate        = opmap negate
      (+)           = liftOp2 (+)
      (*)           = (|><|><|)
      fromInteger i = Operator Nothing $ map ((*fromInteger i) . ithKet) [0..]
      abs           = opmap abs
      signum        = opmap signum

instance Show Operator where
    show (Operator  Nothing  es)        = error "showing operator without basis"
    show (Operator (Just bO) es)
        | any (isNothing . ketBasis) es = error "showing operator without basis"
        | otherwise                     = unlines $ zipWith showCol bO es
        where
            showCol :: Double -> Ket -> String
            showCol x = unlines . map ((show x++" ")++) . lines . show



opRows :: Operator -> [Ket]
opRows op = map (Ket (opBasisOuter op)) . transpose . map ketElems $ opCols op



opmap :: (Ket -> Ket) -> Operator -> Operator
opmap f o = o{ opCols = f`map`opCols o }

liftOp2 :: (Ket -> Ket -> Ket) -> Operator -> Operator -> Operator
liftOp2 f (Operator bO es) (Operator bO' es') = Operator
                                                (headMay $ catMaybes [bO,bO'])
                                                (zipWith f es es')



(.|><|) :: Scalar -> Operator -> Operator
s.|><|o  = (s.|>)`opmap`o

(|><|>) :: Operator -> Ket -> Ket
o|><|>k  = timesDelta . sum $ zipWith (.|>) (ketElems k) (opCols o)

(|><|) :: Ket -> Ket -> Operator
k|><|k' = Operator
          { opBasisOuter = ketBasis k'
          , opCols       = (.|>k)`map`ketElems((<|)k')
          }

(|><|><|) :: Operator -> Operator -> Operator
o|><|><|o' = let b = fromJust $ opBasisOuter o'
              in timesDelta $ Operator (Just b) (getCol`map`[0..length b-1])
    where
        getCol :: Int -> Ket
        getCol c = let b = fromJust $ opBasisOuter o
                    in Ket (Just b) ((`getElem`c)`map`[0..length b-1])

        getElem :: Int -> Int -> Scalar
        getElem r c = sum $ zipWith (*) (map ketElems (opRows o )!!r)
                                        (map ketElems (opCols o')!!c)



trace :: Operator -> Scalar
trace o = (fromReal (delta o)*) . sum $ diag o

diag :: Operator -> [Scalar]
diag = diagRec 0 . map ketElems . opCols
    where
        diagRec :: Int -> [[Scalar]] -> [Scalar]
        diagRec i (xs:xss) = xs!!i : diagRec (i+1) xss
        diagRec _  _       = []
