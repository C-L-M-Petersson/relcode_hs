module Maths.Interpolate where

import           Data.List
import           Data.Maybe

import           Maths.HilbertSpace
import           Maths.HilbertSpace.Distribution


interpolateList :: (Eq a,Ord a,Fractional b) => (a -> b) -> [a] -> [a] -> [b]
                                                                       -> [b]
interpolateList f (x:xs) (x_:x_':x_s) (y:y':ys)
    | x< x_     = 0                                : t0
    | x==x_     = y                                : t0
    | x< x_'    = y+(y'-y)*(f x-f x_)/(f x_'-f x_) : t0
    | otherwise =                                    t1
   where
        t0  = interpolateList f    xs  (x_:x_':x_s) (y:y':ys)
        t1  = interpolateList f (x:xs) (   x_':x_s) (  y':ys)
interpolateList _  xs     _         _ = replicate (length xs) 0

interpolateKet :: [Double] -> Ket -> Ket
interpolateKet xs k
    | isNothing(basis k) = error "cannot interpolate ket without basis"
    | otherwise          = let newElems = interpolateList fromReal xs
                                            (fromJust $ basis k) (ketElems k)
                            in Ket (ketBasisUnit k) (Just xs) newElems



changeGridSize :: (Enum a,Eq a,Ord a,Fractional a,Fractional b) => (a -> b)
                                                    -> Int -> [a] -> [b] -> [b]
changeGridSize f x_N xs ys = interpolateList f x_s xs ys
    where
        x_s   = takeWhile (<x_Max) $ map ((+x_Min) . (*dx_)) [0..]
        dx_   = (x_Max-x_Min)/(fromIntegral x_N-1)
        x_Min = head xs
        x_Max = last xs

changeKetGridSize :: Int -> Ket -> Ket
changeKetGridSize xN k = interpolateKet xs k
    where
        xs   = takeWhile (<xMax) $ map ((+xMin) . (*dx)) [0..]
        dx   = (xMax-xMin)/(fromIntegral xN-1)
        xMin = fromJust $ basisHead k
        xMax = fromJust $ basisTail k
