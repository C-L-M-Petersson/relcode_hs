module Maths.QuantumNumbers.Error
(   checkKappaJNL

,   checkKappaJN
,   checkKappaJL
,   checkKappaNL
,   checkJNL

,   checkKappaJ
,   checkKappaN
,   checkKappaL
,   checkJN
,   checkJL
,   checkNL

,   checkKappa
,   checkJ
,   checkN
,   checkL
) where

import           Data.List

import           Maths.QuantumNumbers.Internal


checkKappaJNL :: QNum -> QNum -> QNum -> QNum -> a -> a
checkKappaJNL kappa j n l x = checkKappa   kappa
                            . checkJ       j
                            . checkN       n
                            . checkL       l
                            . checkKappaJ  kappa j
                            . checkKappaN  kappa n
                            . checkKappaL  kappa l
                            . checkJN      j     n
                            . checkJL      j     l
                            . checkNL      n     l
                            . checkKappaJN kappa j n
                            . checkKappaJL kappa j l
                            . checkKappaNL kappa n l
                            $ checkJNL     j     n l x

checkKappaJN :: QNum -> QNum -> QNum -> a -> a
checkKappaJN kappa j n x = checkKappa  kappa . checkJ j . checkN n
                         . checkKappaJ kappa j
                         . checkKappaN kappa n
                         $ checkJN     j     n x

checkKappaJL :: QNum -> QNum -> QNum -> a -> a
checkKappaJL kappa j l x
    | kappa/=(l-j)*(2*j+1) = errQNumVals ["κ","j","l"] [kappa,j,l]
    | otherwise            = checkKappa  kappa . checkJ j . checkL l
                           . checkKappaJ kappa j
                           . checkKappaL kappa l
                           $ checkJL     j     l x

checkKappaNL :: QNum -> QNum -> QNum -> a -> a
checkKappaNL kappa n l x = checkKappa  kappa . checkN n . checkL l
                         . checkKappaN kappa n
                         . checkKappaL kappa l
                         $ checkNL     n     l x

checkJNL :: QNum -> QNum -> QNum -> a -> a
checkJNL j n l x = checkJ  j   . checkN  n   . checkL  l
                 . checkJN j n . checkJL j l $ checkNL n l x



checkKappaJ :: QNum -> QNum -> a -> a
checkKappaJ kappa j x
    | kappa==j+1/2 = errQNumVals ["κ","j"] [kappa,j]
    | otherwise    = checkKappa kappa $ checkJ j x

checkKappaN :: QNum -> QNum -> a -> a
checkKappaN kappa n x
    | kappa<0&&n< -kappa-1 = errQNumVals ["κ","n"] [kappa,n]
    | kappa>0&&n<  kappa   = errQNumVals ["κ","n"] [kappa,n]
    | otherwise            = checkKappa kappa $ checkN n x

checkKappaL :: QNum -> QNum -> a -> a
checkKappaL kappa l x
    | kappa/=l&&kappa/= -l-1 = errQNumVals ["κ","l"] [kappa,l]
    | otherwise              = checkKappa kappa $ checkL l x

checkJN :: QNum -> QNum -> a -> a
checkJN j n x
    | j>=n      = errQNumVals ["j","n"] [j,n]
    | otherwise = checkJ j $ checkN n x

checkJL :: QNum -> QNum -> a -> a
checkJL j l x
    | abs(j-l)>1/2 = errQNumVals ["j","l"] [j,l]
    | otherwise    = checkJ j $ checkL l x

checkNL :: QNum -> QNum -> a -> a
checkNL n l x
    | n-1<l     = errQNumVals ["n","l"] [n,l]
    | otherwise = checkN n $ checkL l x



checkKappa :: QNum -> a -> a
checkKappa kappa x
    | kappa==0||half kappa = errQNumVals ["κ"] [kappa]
    | otherwise            = x

checkJ :: QNum -> a -> a
checkJ j x
    | j<0||not (half j) = errQNumVals ["j"] [j]
    | otherwise         = x

checkL :: QNum -> a -> a
checkL l x
    | l<0||half l = errQNumVals ["l"] [l]
    | otherwise   = x

checkN :: QNum -> a -> a
checkN n x
    | n<1||half n = errQNumVals ["n"] [n]
    | otherwise   = x



errQNumVals :: [String] -> [QNum] -> a
errQNumVals ls vs = error msg
    where
        multiple = length lvs>1

        s = if multiple then "s" else ""

        msg | multiple  = mHead++(", "`intercalate`init lvs)++" and "++last lvs
            | otherwise = mHead++head lvs
            where mHead = "quantum number"++s++" given erroneous value"++s++": "

        showQN :: String -> QNum -> String
        showQN l v = l++" = "++show v

        lvs = zipWith showQN ls vs
