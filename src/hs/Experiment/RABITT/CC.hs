module Experiment.RABITT.CC
(   calcCCPhaseFromPhases
) where


calcCCPhaseFromPhases :: Num a => [a] -> [a] -> [a]
calcCCPhaseFromPhases phaseWs phaseAts = zipWith (-) phaseAts phaseWs
