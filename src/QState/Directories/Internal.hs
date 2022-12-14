module QState.Directories.Internal where

import           Maths.QuantumNumbers

import           QState.Configure


runDir :: CDict -> FilePath
runDir = cDictOption "runDir"

hfDir :: CDict -> FilePath
hfDir cDict = cDictOption "runDir" cDict++"/hf_wavefunctions/"

pertDir :: QNum -> QNum -> CDict -> FilePath
pertDir kappa n cDict = cDictOption "runDir" cDict
                ++"/pert_"++show kappa++"_"++show (n-lFromKappa kappa)++"/"
