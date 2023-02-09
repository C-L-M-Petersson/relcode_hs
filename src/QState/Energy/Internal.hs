module QState.Energy.Internal where

import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar

import           QState.Configure.Internal
import           QState.Units
import           QState.Units.Internal


type Pulse = Double -> Scalar


omega0 :: EnergyUnit -> CDict -> Double
omega0 eu cDict = eu`from`cDictReadOption "omega0" cDict

fwhm   :: EnergyUnit -> CDict -> Double
fwhm   eu cDict = eu`from`cDictReadOption "fwhm"   cDict


pulsePhase :: EnergyUnit -> CDict -> Double -> Double
pulsePhase eu cDict omega = 0`polyRec`cDictReadOption "phasePolynomial" cDict
    where
        dOmega = to eu $ omega-omega0 eu cDict

        polyRec :: Double -> [Double] -> Double
        polyRec i (a:as) = a*dOmega**i + polyRec (i+1) as
        polyRec _  _     = 0



xuv :: EnergyUnit -> CDict -> Pulse
xuv eu cDict omega
    | omega0 eu cDict==0 = 1
    | otherwise          = exp( -4*log 2*dOmega**2/fwhm'**2 )*phaseFact
    where
        dOmega    = fromReal $ omega-omega0 eu cDict
        fwhm'     = fromReal $ fwhm         eu cDict
        phaseFact = exp(i $ pulsePhase eu cDict omega)

xuvKet :: [Double] -> EnergyUnit -> CDict -> Ket
xuvKet os eu cDict = Ket (Just Energy) (Just os) (xuv eu cDict`map`os)



nEs :: CDict -> Int
nEs = cDictReadOption "nEs"

eKinMin :: EnergyUnit -> CDict -> Double
eKinMin eu cDict = eu`from`cDictReadOption "eKinMin" cDict

eKinMax :: EnergyUnit -> CDict -> Double
eKinMax eu cDict = eu`from`cDictReadOption "eKinMax" cDict

eKinGrid :: EnergyUnit -> CDict -> [Double]
eKinGrid eu cDict = takeWhile (<=eMax) $ map ((+eMin) . (*de)) [0..]
    where
        de   = (eMax-eMin)/(fromIntegral eN-1)
        eN   = nEs cDict
        eMin = eKinMin eu cDict
        eMax = eKinMax eu cDict
