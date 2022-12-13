module Light where

import           Maths.HilbertSpace


type Pulse = Scalar -> Scalar



gaussianXUV :: Double -> Double -> Pulse
gaussianXUV omega0 fwhm (omega:+_) = exp( -4*log 2*(omega-omega0)**2/fwhm**2 )
