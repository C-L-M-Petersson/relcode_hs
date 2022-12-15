module Maths.QuantumNumbers.Internal where


data QNum = QNum { base :: Int
                 , half :: Bool
                 } deriving(Eq)

instance Fractional QNum where
    QNum b False/QNum 2 False = QNum (b`div`2) (odd b)
    _           /_            = errNonHalfInteger
    fromRational r
        | abs(r-floored    )<lim = QNum (floor r) False
        | abs(r-floored-1/2)<lim = QNum (floor r) True
        | otherwise              = errNonHalfInteger
        where
            floored = fromInteger (round r)
            lim     = 1e-3

instance Num QNum where
    QNum b h+QNum b' h'
        | h&&h'       = QNum (b+b'+1) False
        | otherwise   = QNum (b+b'  ) (h||h')
    QNum b h*QNum b' h'
        | not (h||h') = QNum (b*b') h
        | h&&h'       = errNonHalfInteger
        | h'          = QNum (b*b'+(b `div`2)) (odd b )
        | h           = QNum (b*b'+(b'`div`2)) (odd b')
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
    readsPrec _ str = [(uncurry QNum $ parseStr str,"")]
        where parseStr xs
                | take 2 (reverse xs)=="2/" = ((read xs'-1)`div`2,True )
                | otherwise                 = ( read xs          ,False)
                where xs' = reverse . drop 2 $ reverse xs

instance Show QNum where
    show (QNum b h)
        | h         = show (2*b+1)++"/2"
        | otherwise = show b

errNonHalfInteger :: a
errNonHalfInteger = error "can only treat whole or half integer quantum numbers"



intFromQNum :: QNum -> Int
intFromQNum (QNum base False) = base
intFromQNum  q                = error
    $ "cannot convert "++show (doubleFromQNum q)++" to int"

doubleFromQNum :: QNum -> Double
doubleFromQNum (QNum base True ) = fromIntegral base+0.5
doubleFromQNum (QNum base False) = fromIntegral base



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
