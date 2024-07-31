module QState.Energy.Internal
(   Pulse
,   PulseType

,   omega0
,   fwhm

,   pulsePhase

,   xuv
,   xuvKet

,   energyKet

,   nEs
,   eGridMin
,   eGridMax
,   eGrid
) where

import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar

import           QState.Configure.Internal
import           QState.Units


type Pulse = Double -> Scalar

data PulseType = None | Gaussian deriving(Read)


omega0 :: EnergyUnit -> CDict -> Double
omega0 eu cDict = eu`from`cDictReadOption "omega0" cDict

fwhm   :: EnergyUnit -> CDict -> Double
fwhm   eu cDict = eu`from`cDictReadOption "fwhm"   cDict


pulsePhase :: EnergyUnit -> CDict -> Double -> Double
pulsePhase eu cDict omega = 0`polyRec`cDictReadOption "phasePolynomial" cDict
    where
        dOmega = to eu $ omega-omega0 eu cDict

        polyRec :: Double -> [Double] -> Double
        polyRec ind (a:as) = a*dOmega**ind + polyRec (ind+1) as
        polyRec _   _      = 0



xuv :: EnergyUnit -> CDict -> Pulse
xuv eu cDict omega = case (cDictReadOption "pulseType" cDict) of
        None     -> 1
        Gaussian -> exp( -4*log 2*dOmega**2/fwhm'**2 )*phaseFact
    where
        dOmega    = fromReal $ omega-omega0 eu cDict
        fwhm'     = fromReal $ fwhm         eu cDict
        phaseFact = exp(i $ pulsePhase eu cDict omega)

xuvKet :: [Double] -> EnergyUnit -> CDict -> Ket
xuvKet os eu cDict = ket (Just Energy) (Just os) (xuv eu cDict`map`os)


energyKet :: [Double] -> [Scalar] -> Ket
energyKet es cs = ket (Just Energy) (Just es) cs



nEs :: CDict -> Int
nEs = cDictReadOption "nEs"

eGridMin :: EnergyUnit -> CDict -> Double
eGridMin eu cDict = eu`from`cDictReadOption "eGridMin" cDict

eGridMax :: EnergyUnit -> CDict -> Double
eGridMax eu cDict = eu`from`cDictReadOption "eGridMax" cDict

eGrid :: EnergyUnit -> CDict -> [Double]
eGrid eu cDict = takeWhile (<=eMax) $ map ((+eMin) . (*de)) [0..]
    where
        de   = (eMax-eMin)/(fromIntegral eN-1)
        eN   = nEs cDict
        eMin = eGridMin eu cDict
        eMax = eGridMax eu cDict
