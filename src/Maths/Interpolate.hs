module Maths.Interpolate where

import           Data.Maybe

import           Maths.HilbertSpace


interpolateList :: (Eq a,Ord a,Fractional b) => (a -> b) -> [a] -> [a] -> [b]
                                                                       -> [b]
interpolateList f (x:xs) (x_:x_':x_s) (y:y':ys)
    | x< x_      = 0                                : t0
    | x==x_      = y                                : t0
    | x< x_'     = y+(y'-y)*(f x-f x_)/(f x_'-f x_) : t0
    | otherwise =                                     t1
   where
        t0  = interpolateList f    xs  (x_:x_':x_s) (y:y':ys)
        t1  = interpolateList f (x:xs) (   x_':x_s) (  y':ys)
interpolateList _  xs     _         _ = replicate (length xs) 0

interpolateKet :: [Double] -> Ket -> Ket
interpolateKet xs k
    | isNothing(basis k) = error "cannot interpolate ket without basis"
    | otherwise          = let newElems = interpolateList fromReal xs
                                            (fromJust $ basis k) (ketElems k)
                            in Ket (Just xs) newElems
