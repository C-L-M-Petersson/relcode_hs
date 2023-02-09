module Maths.HilbertSpace.Evolving where

import           Data.Maybe

import           GHC.Data.Maybe

import           Safe

import           QState
import           QState.Units


data Evolving a = Evolving
                  { evolvingInUnit :: Maybe UnitType
                  , xs             :: [Double]
                  , valueAt        :: [a]
                  }

instance HasUnit a => HasUnit (Evolving a) where
    unitType = evolvingInUnit
    toUnits   (Evolving mUT xs vAt) = Evolving mUT<$>xs'<*>vAt'
        where
            xs' = case mUT of Nothing -> return xs
                              Just u  -> (`map`xs) . to  <$>getUnit u
            vAt' = case mUT of Nothing -> return vAt
                               Just u  -> mapM toUnits   vAt
    fromUnits (Evolving mUT xs vAt) = Evolving mUT<$>xs'<*>vAt'
        where
            xs' = case mUT of Nothing -> return xs
                              Just u  -> (`map`xs) . from<$>getUnit u
            vAt' = case mUT of Nothing -> return vAt
                               Just u  -> mapM fromUnits vAt

instance Num a => Num (Evolving a) where
      negate      = emap negate
      (+)         = liftEvolving2 "(+)" (+)
      (*)         = liftEvolving2 "(*)" (*)
      --fromInteger = Evolving Nothing [] . const . fromInteger
      abs         = emap abs
      signum      = emap signum

instance Show a => Show (Evolving a) where
    show (Evolving _ xs valueAt) = unlines $ zipWith showVal xs valueAt
        where
            showVal :: Show a => Double -> a -> String
            showVal x v = unlines $ ((show x++" ")++)<$>lines (show v)

--instance Show Ket where
--    show (Ket Nothing   Nothing  es) = ("|"++) . (++">") . intercalate ","
--                                     $ map show es
--    show (Ket _         (Just b) es) = unlines $ zipWith showElem b es
--        where showElem b e = unwords $ map show [b,absVal e,phase e]
--


emap :: (a -> a) -> Evolving a -> Evolving a
emap f e = e{ valueAt = map f $ valueAt e }

liftEvolving2 :: String -> (a -> a -> a) -> Evolving a -> Evolving a
                                                                -> Evolving a
liftEvolving2 opName f (Evolving mUT xs vAt) (Evolving mUT' xs' vAt')
    | xs==xs'   = Evolving (mUT`firstJust`mUT') xs (zipWith f vAt vAt')
    | otherwise = error $ "Cannot use "++opName++" operator on functions "
                        ++"evolving on different bases"



evolving :: Maybe UnitType -> [Double] -> (Double -> a) -> Evolving a
evolving mUT xs f = Evolving mUT xs (map f xs)
