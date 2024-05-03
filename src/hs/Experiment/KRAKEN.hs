module Experiment.KRAKEN
(   kraken

,   kraken1ph
,   kraken1phForQNums

,   kraken2ph
,   kraken2phForQNums
) where

import           Experiment.KRAKEN.OnePhoton
import           Experiment.KRAKEN.TwoPhoton

import           QState


kraken :: QState()
kraken = kraken1ph>>kraken2ph
