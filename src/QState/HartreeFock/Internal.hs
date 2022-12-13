module QState.HartreeFock.Internal where

import           Maths.QuantumNumbers

import           QState.Internal.Configure
import           QState.Internal.Input.Directories


hfEnergy :: QNum -> QNum -> CDict -> IO Double
hfEnergy kappa n cDict = read . head . words
                       . (!!intFromQNum(kappa`nthKappaElevel`n))
                       . lines<$>readFile fp
    where fp = hfDir cDict++"/hf_energies_kappa_"++show kappa++".dat"
