module QState.FilePath.Internal where

import           Data.Composition
import           Data.List.Split
import           Data.Tuple.HT

import           Maths.QuantumNumbers

import           System.Directory
import           System.FilePath

import           QState.Configure.Internal


cDictFilePath :: String -> CDict -> FilePath
cDictFilePath key cDict = concat
                        . mapTail (uncurry replace . mapSnd tail . span (/='}'))
                        . splitOn "${" $ cDictOption key cDict
    where
        replace key' str = cDictOption key' cDict++str
        mapTail f (x:xs) = x:map f xs
        tail' [] = []
        tail' xs = tail xs

createCDictParentDir :: String -> CDict -> IO()
createCDictParentDir = createDirectoryIfMissing True . takeDirectory
                     .:cDictFilePath



runDir :: CDict -> FilePath
runDir = cDictFilePath "runDir"

hfDir :: CDict -> FilePath
hfDir cDict = cDictFilePath "runDir" cDict++"/hf_wavefunctions/"

pertDir :: QNum -> QNum -> CDict -> FilePath
pertDir kappa n cDict = cDictFilePath "runDir" cDict
                ++"/pert_"++show kappa++"_"++show (n-lFromKappa kappa)++"/"

secondPhotonDir :: CDict -> FilePath
secondPhotonDir cDict = cDictFilePath "runDir" cDict++"/second_photon/"
