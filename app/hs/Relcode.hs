module Main (main) where

import           Experiment.CrossSections
import           Experiment.KRAKEN
import           Experiment.RABITT

import           QState.Run


main :: IO()
main = runQState $ crossSection1ph>>kraken>>rabitt
