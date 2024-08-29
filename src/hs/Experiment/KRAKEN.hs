module Experiment.KRAKEN
(   kraken
,   kraken1ph
,   kraken2ph
) where

import           Experiment.KRAKEN.OnePhoton
import           Experiment.KRAKEN.TwoPhoton

import           QState


kraken :: QState()
kraken = kraken1ph>>kraken2ph
