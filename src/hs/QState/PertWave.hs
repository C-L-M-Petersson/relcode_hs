module QState.PertWave
(   waveNumberE
,   waveNumberEs

,   xi
,   getXi
,   timesXi

,   subtractedPhaseFactor
,   subtractedPhase
) where

import           Control.Monad.Extra

import           Data.Maybe

import           Maths.HilbertSpace.Scalar
import           Maths.QuantumNumbers

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

getXi :: QNum -> QNum -> QState Scalar
getXi kappa mJ = getReadOption "angleRABITT">>= \mTheta ->
    return $ if isJust mTheta then xi kappa mJ (fromJust mTheta) 0 else 1

timesXi :: QNum -> QNum -> [Scalar] -> QState [Scalar]
timesXi kappa2 mJ xs = (`map`xs) . (*)<$>getXi kappa2 mJ
