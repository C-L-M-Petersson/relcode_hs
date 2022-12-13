import           Maths.HilbertSpace
import           Maths.HilbertSpace.DensityMatrix
import           Maths.Interpolate
import           Maths.QuantumNumbers

import           QState
import           QState.Directories
import           QState.HartreeFock
import           QState.OnePhoton
import           QState.Run


main :: IO()
main = runQState test
--print $ lFromKappa (0)

test :: QState ()
test = do

    --getPertDir (-1) 3>>=liftIO.print
    --getPertDir (-2) 3>>=liftIO.print
    --getPertDir ( 1) 3>>=liftIO.print
    liftIO$putStrLn ""
    getHFEnergy (-2) 2     >>=liftIO.print
    getOmegas   (-2) 3     >>=liftIO.print
    liftIO$putStrLn ""
    getPhaseF   (-2) 3 (-1)>>=liftIO.print
    getPhaseF   (-2) 3 ( 2)>>=liftIO.print
    getPhaseF   (-2) 3 (-3)>>=liftIO.print

--main = print $ kappaFromLJ 1 (1/2)
--(read "3/2"::QNum)
--test--12-- $ kappaFromJL (3/2) 2
--
-- 2 3/2
-- 1 1/2
---1 1/2
---2 3/2
---3 5/2
