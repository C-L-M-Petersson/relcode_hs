module QState.HartreeFock.Internal where

import           Maths.QuantumNumbers

import           QState.Configure
import           QState.Directories.Internal


hfEnergy :: QNum -> QNum -> CDict -> IO Double
hfEnergy kappa n cDict = read . head . words
                       . (!!intFromQNum(nthKappaElevel kappa n-1))
                       . lines<$>readFile fp
    where fp = hfDir cDict++"/hf_energies_kappa_"++show kappa++".dat"



groundStateShift :: QNum -> QNum -> [Double] -> CDict -> IO [Double]
groundStateShift kappa n es cDict = (`map`es) . (+)<$>hfEnergy kappa n cDict
