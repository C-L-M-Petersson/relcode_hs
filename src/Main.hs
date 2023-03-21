import           Experiment.CrossSections
import           Experiment.KRAKEN

import           QState
import           QState.Configure
import           QState.Run


main :: IO()
main = runQState $ crossSections1ph>>kraken1ph>>kraken2ph
