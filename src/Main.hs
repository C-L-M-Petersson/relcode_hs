import           Control.Monad
import           Experiment.KRAKEN
import           Maths.HilbertSpace
--import           Maths.HilbertSpace.State
import           Maths.HilbertSpace.DensityMatrix
import           Maths.Interpolate
import           Maths.QuantumNumbers

import           QState
import           QState.Energy
import           QState.Output
import           QState.Utility
import           QState.Utility.Internal
import           QState.FilePath
import           QState.HartreeFock
import           QState.OnePhoton
import           QState.OnePhoton.Internal
import           QState.Run


main :: IO()
main = runQState $ kraken1ph
