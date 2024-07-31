module Maths.HilbertSpace.Operator
(   Operator

,   opRows
,   opElems
,   opElem

,   (.|><|)
,   (|><|>)
,   (|><|)
,   (|><|><|)

,   trace
,   diag
,   offDiag
,   reverseCols
) where

import           Data.List                       (transpose)

import           GHC.Data.Maybe

import           Maths.HilbertSpace.Distribution
import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar

import           QState
import           QState.Units


data Operator = Operator { opBasisUnitOuter :: Maybe UnitType
                         , opBasisOuter     :: Maybe [Double]
                         , opCols           :: [Ket]
                         }

instance Distributed Operator where
    norm       o = assertReal (trace o)
    scale        = (.|><|) . fromReal
    basis        = opBasisOuter
    setBasis b o = o{ opBasisOuter = b, opCols = setBasis b`map`opCols o }

instance HasUnit Operator where
    unitType = opBasisUnitOuter
    toUnits   (Operator mUT mBO cs) = Operator mUT<$>mBO'<*>mapM toUnits   cs
        where mBO' = case (mUT,mBO) of
                (Just ut,Just bo) -> Just . (`map`bo) . to  <$>getUnit ut
                _                 -> return mBO
    fromUnits (Operator mUT mBO cs) = Operator mUT<$>mBO'<*>mapM fromUnits cs
        where mBO' = case (mUT,mBO) of
                (Just ut,Just bo) -> Just . (`map`bo) . from<$>getUnit ut
                _                 -> return mBO
    setUnit ut op                   = op{ opBasisUnitOuter = Just ut
                                        , opCols           = map (setUnit ut)
                                                           $ opCols op
                                        }

instance Num Operator where
      negate        = opmap negate
      (+)           = liftOp2 (+)
      (*)           = (|><|><|)
      fromInteger n = Operator Nothing Nothing
                    $ map ((*fromInteger n) . ithKet) [0..]
      abs           = opmap abs
      signum        = opmap signum

instance Show Operator where
    show (Operator _  Nothing  _ )      = error "showing operator without basis"
    show (Operator _ (Just bo) es)
        | any (isNothing . ketBasis) es = error "showing operator without basis"
        | otherwise                     = unlines $ zipWith showCol bo es
        where
            showCol :: Double -> Ket -> String
            showCol x = unlines . map ((show x++" ")++) . lines . show



opRows :: Operator -> [Ket]
opRows op = map (ket (opBasisUnitOuter op) (opBasisOuter op))
                                        . transpose . map ketElems $ opCols op

opElems :: Operator -> [[Scalar]]
opElems = map ketElems . opRows

opElem :: Operator -> Int -> Int -> Scalar
opElem op r c = (opElems op!!c)!!r



opmap :: (Ket -> Ket) -> Operator -> Operator
opmap f o = o{ opCols = f`map`opCols o }

liftOp2 :: (Ket -> Ket -> Ket) -> Operator -> Operator -> Operator
liftOp2 f (Operator mUT mBO es) (Operator mUT' mBO' es') = Operator
                                                (mUT`firstJust`mUT')
                                                (mBO`firstJust`mBO')
                                                (zipWith f es es')



(.|><|) :: Scalar -> Operator -> Operator
s.|><|o  = (s.|>)`opmap`o

(|><|>) :: Operator -> Ket -> Ket
o|><|>k  = timesDelta . sum $ zipWith (.|>) (ketElems k) (opCols o)

(|><|) :: Ket -> Ket -> Operator
k|><|k' = Operator
          { opBasisUnitOuter = ketBasisUnit k'
          , opBasisOuter     = ketBasis k'
          , opCols           = (.|>k)`map`ketElems((<|)k')
          }

(|><|><|) :: Operator -> Operator -> Operator
o|><|><|o' = let mut =            opBasisUnitOuter o'
                 b   = fromJust $ opBasisOuter     o'
              in timesDelta $ Operator mut (Just b) (getCol`map`[0..length b-1])
    where
        getCol :: Int -> Ket
        getCol c = let mut =            opBasisUnitOuter o
                       b   = fromJust $ opBasisOuter o
                    in ket mut (Just b) ((`getElem`c)`map`[0..length b-1])

        getElem :: Int -> Int -> Scalar
        getElem r c = sum $ zipWith (*) (map ketElems (opRows o )!!r)
                                        (map ketElems (opCols o')!!c)



trace :: Operator -> Scalar
trace o = (fromReal (delta o)*) . sum $ diag o

diag :: Operator -> [Scalar]
diag = diagRec 0 . map ketElems . opCols
    where
        diagRec :: Int -> [[Scalar]] -> [Scalar]
        diagRec ind (xs:xss) = xs!!ind : diagRec (ind+1) xss
        diagRec _    _       = []

offDiag :: Operator -> Int -> [Scalar]
offDiag o c
    | c==0      = diag o
    | c> 0      = offDiagRec 0 . map ketElems . drop c   $ opCols o
    | otherwise = offDiagRec 0 . map (drop c . ketElems) $ opCols o
    where
        offDiagRec :: Int -> [[Scalar]] -> [Scalar]
        offDiagRec ind (xs:xss)
            | ind>=length xs = []
            | otherwise      = xs!!ind : offDiagRec (ind+1) xss
        offDiagRec _  _      = []

reverseCols :: Operator -> Operator
reverseCols op = op{ opCols = reverse $ opCols op }
