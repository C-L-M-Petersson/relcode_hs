{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Maths.HilbertSpace.Evolving
(   Evolving
,   evolving
) where

import           GHC.Data.Maybe

import           QState
import           QState.Units


data Evolving a = Evolving
                  { evolvingInUnit_ :: Maybe UnitType
                  , mXs_            :: Maybe [Double]
                  , valueAt_        :: [a]
                  }

instance HasUnit a => HasUnit (Evolving a) where
    unitType = evolvingInUnit_
    toUnits   (Evolving mUT mXs vAt) = Evolving mUT<$>mXs'<*>vAt'
        where
            mXs' = case (mUT,mXs) of
                (Just u ,Just xs) -> Just . (`map`xs) . to <$>getUnit u
                _                 -> return mXs
            vAt' = case mUT of Just _  -> mapM toUnits vAt
                               Nothing -> return vAt
    fromUnits (Evolving mUT mXs vAt) = Evolving mUT<$>mXs'<*>vAt'
        where
            mXs' = case (mUT,mXs) of
                (Just u ,Just xs) -> Just . (`map`xs) . from<$>getUnit u
                _                 -> return mXs
            vAt' = case mUT of Just _  -> mapM fromUnits vAt
                               Nothing -> return vAt

instance Num a => Num (Evolving a) where
      negate      = emap negate
      (+)         = liftEvolving2 "(+)" (+)
      (*)         = liftEvolving2 "(*)" (*)
      fromInteger = Evolving Nothing Nothing . repeat . fromInteger
      abs         = emap abs
      signum      = emap signum

instance Show a => Show (Evolving a) where
    show (Evolving _  Nothing  vAt) = show $ "_ -> "++show (head vAt)
    show (Evolving _ (Just xs) vAt) = unlines $ zipWith showVal xs vAt
        where
            showVal :: Show a => Double -> a -> String
            showVal x v = unlines $ ((show x++" ")++)<$>lines (show v)

--instance Show Ket where
--    show (Ket Nothing   Nothing  es) = ("|"++) . (++">") . intercalate ","
--                                     $ map show es
--    show (Ket _         (Just b) es) = unlines $ zipWith showElem b es
--        where showElem b e = unwords $ map show [b,absVal e,phase e]

evolving :: Maybe UnitType -> [Double] -> (Double -> a) -> Evolving a
evolving mUT xs f = Evolving mUT (Just xs) (map f xs)


--TODO: Implement this in O(1)
sameBasis :: Evolving a -> Evolving b -> Bool
sameBasis (Evolving _ mXs _) (Evolving _ mXs' _) = case (mXs,mXs') of
    (Just xs,Just xs') -> xs==xs'
    _                  -> True


emap :: (a -> a) -> Evolving a -> Evolving a
emap f e = e{ valueAt_ = map f $ valueAt_ e }

liftEvolving2 :: String -> (a -> b -> c) -> Evolving a -> Evolving b
                                                                -> Evolving c
liftEvolving2 opName f (Evolving mUT mXs vAt) (Evolving mUT' mXs' vAt') =
        if Evolving mUT mXs vAt`sameBasis`Evolving mUT' mXs' vAt'
    then Evolving (mUT`firstJust`mUT') (mXs`firstJust`mXs') (zipWith f vAt vAt')
    else error $ "Cannot use "++opName++" operator on functions evolving on"
                                                        ++" different bases"
