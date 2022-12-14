module QState.Light.Internal where

import                Maths.HilbertSpace.Ket
import                Maths.HilbertSpace.Scalar

import                QState.Configure
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
