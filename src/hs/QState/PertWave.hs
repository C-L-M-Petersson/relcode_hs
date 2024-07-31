module QState.PertWave
(   waveNumberE
,   waveNumberEs

,   subtractedPhaseFactor
,   subtractedPhase

,   xi
) where

import           Control.Monad.Extra

import           QState
import           QState.Configure
import           QState.PertWave.Internal
import           QState.PertWave.NonRelativistic as NonRel
import           QState.PertWave.Relativistic    as Rel


waveNumberE :: Double -> QState Double
waveNumberE e = ifM (getReadOption "useRelativisitcWaveNum")
                                                (return $    Rel.waveNumber e)
                                                (return $ NonRel.waveNumber e)

waveNumberEs :: [Double] -> QState [Double]
waveNumberEs = mapM waveNumberE
