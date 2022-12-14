module Maths.HilbertSpace.Operator where

import           Data.Maybe

import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar

import           Safe


data Operator = Operator { basisOuter :: Maybe [Double]
                         , opElems    :: [Ket]
                         }

instance Num Operator where
      negate        = opmap negate
      (+)           = liftOp2 (+)
      (*)           = liftOp2 (*)
      fromInteger i = Operator Nothing $ map ((*fromInteger i) . ithKet) [0..]
      abs           = opmap abs
      signum        = opmap signum

instance Show Operator where
    show (Operator  Nothing  es)     = error "showing operator without basis"
    show (Operator (Just bO) es)
        | any (isNothing . basis) es = error "showing operator without basis"
        | otherwise                  = unlines $ zipWith showCol bO es
        where
            showCol :: Double -> Ket -> String
            showCol x = unlines . map ((show x++" ")++) . lines . show



opmap :: (Ket -> Ket) -> Operator -> Operator
opmap f o = o{ opElems = f`map`opElems o }

liftOp2 :: (Ket -> Ket -> Ket) -> Operator -> Operator -> Operator
liftOp2 f (Operator bO es) (Operator bO' es') = Operator
                                                (headMay $ catMaybes [bO,bO'])
                                                (zipWith f es es')



(|><|>) :: Operator -> Ket -> Ket
o|><|>k  = sum $ zipWith (.|>) (ketElems k) (opElems o)

(|><|) :: Ket -> Ket -> Operator
k|><|k' = Operator
          { basisOuter = basis k'
          , opElems    = (.|>k)`map`ketElems((<|)k')
          }



trace :: Operator -> Scalar
trace = sum . traceRec 0 . map ketElems . opElems
    where
        traceRec :: Int -> [[Scalar]] -> [Scalar]
        traceRec i (xs:xss) = xs!!i : traceRec (i+1) xss
        traceRec _  _       = []
