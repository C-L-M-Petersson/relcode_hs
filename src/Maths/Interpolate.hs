module Maths.Interpolate where


import           Maths.HilbertSpace



interpolateList :: (Eq a,Fractional a,Ord a) => [a] -> [a] -> [a] -> [a]
interpolateList (a:as) (x:x':xs) (y:y':ys)
    | a< x      = 0                     : t0
    | a==x      = y                     : t0
    | a< x'     = y+(y'-y)*(a-x)/(x'-x) : t0
    | otherwise =                         t1
   where
        t0 = interpolateList    as  (x:x':xs) (y:y':ys)
        t1 = interpolateList (a:as) (  x':xs) (  y':ys)
interpolateList    xs   _        _      = replicate (length xs) 0

interpolateKet :: [Scalar] -> [Scalar] -> Ket -> Ket
interpolateKet as xs = Vec . interpolateList as xs . elems
