import           Control.Monad
import           Maths.HilbertSpace
--import           Maths.HilbertSpace.State
import           Maths.HilbertSpace.DensityMatrix
import           Maths.Interpolate
import           Maths.QuantumNumbers

import           QState
import           QState.Utility
import           QState.Utility.Internal
import           QState.Directories
import           QState.HartreeFock
import           QState.OnePhoton
import           QState.OnePhoton.Internal
import           QState.Run


main :: IO()
main = runQState test
--print $ lFromKappa (0)

test :: QState ()
test = do

    -- --getPertDir (-1) 3>>=liftIO.print
    -- --getPertDir (-2) 3>>=liftIO.print
    -- --getPertDir ( 1) 3>>=liftIO.print
    -- liftIO$putStrLn ""
    -- --getHFEnergy (-2) 2     >>=liftIO.print
    -- liftIO$putStrLn ""
    -- getPhaseF   (-2) 3 (-1)>>=liftIO.print
    -- getPhaseF   (-2) 3 ( 2)>>=liftIO.print
    -- getPhaseF   (-2) 3 (-3)>>=liftIO.print

    kraken1photon [-2] [3] [2]
    --kraken1photon [-1] [3] [1]

    --as <- getAmps    1 3 (-1)
    --os <- getOmegas  1 3      -- >>=liftIO . mapM_ print
    ----ms <- getMatElem 1 3 (-1) -- >>=liftIO . mapM_ print
    --ms <- getMatElems [1] [3] [-1] -- >>=liftIO . mapM_ print
    --let cP = map (coulombPhase (-1)) os
    --let k  = map waveNumber os
    ----liftIO$mapM_ print cP
    --liftIO$mapM_ print ms

kraken1photon :: [QNum] -> [QNum] -> [QNum] -> QState()
kraken1photon kappas0 ns0 kappas1 = do
    --liftIO$print [kappas0,ns0,kappas1]
    --getMatElems kappas0 ns0 kappas1>>=liftIO.print
    getExcitedState kappas0 ns0 kappas1>>=liftIO.print . fromState
    --ks  <- zipWithM (\kappa0 n0 -> getExcitedState [kappa0] [n0] kappas1) kappas0 ns0
    ess <- zipWithM getGroundStateShiftedOmegas kappas0 ns0

    --liftio . print $

    --printKet (head ess) (elems $ head ks)--(repeat 0)--(elems $ head ks)
    --print =<<(<$>getMatElems kappas0 ns0 kappas1)
    return()

--printKet :: [Double] -> [Scalar] -> QState()
--printKet (e:es) (v:vs) = liftIO (putStrLn (show e++" "++show (absVal v)++" "++show (phase v)))>>printKet es vs
--printKet  _      _     = return()
