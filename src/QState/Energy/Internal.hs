module QState.Energy.Internal where

import                Maths.HilbertSpace.Ket
import                Maths.HilbertSpace.Scalar

import                QState.Configure.Internal
import                QState.Units.Internal


type Pulse = Double -> Double


omega0 :: EnergyUnit -> CDict -> Double
omega0 eu cDict = eu`from`cDictReadOption "omega0" cDict

fwhm   :: EnergyUnit -> CDict -> Double
fwhm   eu cDict = eu`from`cDictReadOption "fwhm"   cDict



xuv :: EnergyUnit -> CDict -> Pulse
xuv eu cDict omega = exp( -4*log 2*(omega-omega0 eu cDict)**2/fwhm eu cDict**2 )

xuvKet :: [Double] -> EnergyUnit -> CDict -> Ket
xuvKet os eu cDict = Ket (Just os) ((fromReal . xuv eu cDict)`map`os)



nEs :: CDict -> Int
nEs = cDictReadOption "nEs"

eKinMin :: EnergyUnit -> CDict -> Double
eKinMin eu cDict = eu`from`cDictReadOption "eKinMin" cDict

eKinMax :: EnergyUnit -> CDict -> Double
eKinMax eu cDict = eu`from`cDictReadOption "eKinMax" cDict

eKinGrid :: EnergyUnit -> CDict -> [Double]
eKinGrid eu cDict = takeWhile (<eMax) $ map ((+eMin) . (*de)) [0..]
    where
        de   = (eMax-eMin)/(fromIntegral eN-1)
        eN   = nEs cDict
        eMin = eKinMin eu cDict
        eMax = eKinMax eu cDict
