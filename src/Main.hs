import           Experiment.KRAKEN

import           QState.Run


main :: IO()
main = runQState $ kraken1ph>>kraken2ph
