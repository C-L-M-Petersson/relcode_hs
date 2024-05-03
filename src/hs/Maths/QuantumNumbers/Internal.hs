{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Maths.QuantumNumbers.Internal
(   QNum
,   isHalf

,   intFromQNum
,   doubleFromQNum
,   scalarFromQNum

,   kappaFromJL
,   lFromKappa
,   jFromKappa
,   nthKappaElevel

,   reachableKappas
) where

import           Data.AEq

import           Maths.HilbertSpace.Scalar


data QNum = QNum { base_ :: Int
                 , half_ :: Bool
                 } deriving(Eq)

instance Fractional QNum where
    QNum b False/QNum 2 False = QNum (b`div`2) (odd b)
    _           /_            = errNonHalfInteger
    fromRational r
        | d~==floored     = QNum (round r) False
        | d~==floored+1/2 = QNum (round r) True
        | otherwise       = errNonHalfInteger
        where
            floored = fromInteger(floor r)
            d = fromRational r :: Double

instance Num QNum where
    QNum b h+QNum b' h'
        | h&&h'       = QNum (b+b'+1) False
        | otherwise   = QNum (b+b'  ) (h||h')
    QNum b h*QNum b' h'
        | h&&h'       = errNonHalfInteger
        | h'          = QNum (b*b'+(b `div`2)) (odd b )
        | h           = QNum (b*b'+(b'`div`2)) (odd b')
        | otherwise   = QNum (b*b') h
    negate (QNum b h)
        | h           = QNum (-b-1) h
        | otherwise   = QNum (-b  ) h
    abs (QNum b h)
        | b>=0        = QNum   b    h
        | h           = QNum (-b-1) h
        | otherwise   = QNum (-b)   h
    signum (QNum b h)
        | b==0&&h     = QNum  0         False
        | otherwise   = QNum (signum b) False
    fromInteger b     = QNum (fromInteger b) False

instance Ord QNum where
    QNum b h<=QNum b' h'
        | b < b'    = True
        | b > b'    = False
        | h ==h'    = True
        | h'        = True
        | otherwise = False

instance Read QNum where
    readsPrec _ str = let (strHead,strTail) = (`elem`"-0123456789/")`span`str
                       in [(parseStr strHead,strTail)]
        where
            parseStr :: String -> QNum
            parseStr xs
                | take 2 (reverse xs)=="2/" = QNum ((read xs'-1)`div`2) True
                | otherwise                 = QNum ( read xs          ) False
                where xs' = reverse . drop 2 $ reverse xs

instance Show QNum where
    show (QNum b h)
        | h         = show (2*b+1)++"/2"
        | otherwise = show b

isHalf :: QNum -> Bool
isHalf = half_

errNonHalfInteger :: a
errNonHalfInteger = error "can only treat whole or half integer quantum numbers"


intFromQNum :: QNum -> Int
intFromQNum (QNum b False) = b
intFromQNum  q             = error
    $ "cannot convert "++show (doubleFromQNum q)++" to int"

doubleFromQNum :: QNum -> Double
doubleFromQNum (QNum b True ) = fromIntegral b+0.5
doubleFromQNum (QNum b False) = fromIntegral b

scalarFromQNum :: QNum -> Scalar
scalarFromQNum = fromReal . doubleFromQNum


kappaFromJL :: QNum -> QNum -> QNum
kappaFromJL j l = (l-j)*(2*j+1)

lFromKappa :: QNum -> QNum
lFromKappa kappa
    | kappa<0   = -kappa-1
    | otherwise =  kappa

jFromKappa :: QNum -> QNum
jFromKappa kappa = abs kappa-1/2

nthKappaElevel :: QNum -> QNum -> QNum
nthKappaElevel kappa n = n-lFromKappa kappa


reachableKappas :: QNum -> [QNum]
reachableKappas kappa = filter (/=0)
    [kappa-signum kappa,-kappa,kappa+signum kappa]
