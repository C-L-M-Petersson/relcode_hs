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
main = runQState $ kraken1ph[-2,1] [3,3] [-1,2,-3]
--test
--print $ lFromKappa (0)

--test :: QState ()
--test = do
--
--    -- --getPertDir (-1) 3>>=liftIO.print
--    -- --getPertDir (-2) 3>>=liftIO.print
--    -- --getPertDir ( 1) 3>>=liftIO.print
--    -- liftIO$putStrLn ""
--    -- --getHFEnergy (-2) 2     >>=liftIO.print
--    -- liftIO$putStrLn ""
--    -- getPhaseF   (-2) 3 (-1)>>=liftIO.print
--    -- getPhaseF   (-2) 3 ( 2)>>=liftIO.print
--    -- getPhaseF   (-2) 3 (-3)>>=liftIO.print
--
--    --kraken1photon [-2] [3] [2]
--    kraken1photon [-2,1] [3,3] [-1,2,-3]
--    --kraken1photon [-1] [3] [1]
--
--    --as <- getAmps    1 3 (-1)
--    --os <- getOmegas  1 3      -- >>=liftIO . mapM_ print
--    ----ms <- getMatElem 1 3 (-1) -- >>=liftIO . mapM_ print
--    --ms <- getMatElems [1] [3] [-1] -- >>=liftIO . mapM_ print
--    --let cP = map (coulombPhase (-1)) os
--    --let k  = map waveNumber os
--    ----liftIO$mapM_ print cP
--    --liftIO$mapM_ print ms
--
----kraken1photon :: [QNum] -> [QNum] -> [QNum] -> QState()
----kraken1photon kappas0 ns0 kappas1 = do
----
----    --getExcitedStateByOmega       kappas0        ns0  kappas1 >>= liftIO.print -- . fromState
----    --getExcitedStateByEkin  (head kappas0) (head ns0) kappas1 >>= liftIO.print -- . fromState
----    --getExcitedStateByEkin  (-2) (3) kappas1 >>= liftIO.print -- . fromState
----    --
----    --zipWithM ((((flip (uncurry getExcitedStateByEkin) kappas1)::(QNum,QNum)->QState Ket) . (,))
----    --zipWithM ((curry((flip (uncurry getExcitedStateByEkin) kappas1)::(QNum,QNum)->QState Ket))
----    --                ::QNum -> QNum -> QState Ket) kappas0 ns0
----
----
----    --getDensityMatrix kappas0 ns0 kappas1>>=liftIO.print
----    --ks <- zipWithM (curry (flip (uncurry getExcitedStateByEkin) kappas1)) kappas0 ns0
----    --liftIO.print $ fromState (ks!!1)
----
----    --zipWithM (curry (flip (uncurry getExcitedStateByEkin) kappas1)) kappas0 ns0
----    --withCDictM $ liftIO . print
----
----    --getMatElemKet kappas0 ns0 kappas1>>=liftIO . print
----    --getInterpolatedMatElemKet kappas0 ns0 kappas1>>=liftIO . print
----    --zipWithM (curry (flip (uncurry getInterpolatedExcitedStateByEkin) kappas1)) kappas0 ns0
----    --    >>=liftIO . mapM_ print
----
----    --getDensityMatrix kappas0 ns0 kappas1>>=liftIO.print
----    --getDensityMatrix kappas0 ns0 kappas1>>=liftIO.print.purity
----
----    --zipWithM (curry (flip (uncurry getPureState) kappas1)) kappas0 ns0
----    --    >>=liftIO . mapM_ print
----    return()
----
----    --k <- getExcitedState kappas0 ns0 kappas1 -- >>=liftIO.print . fromState
----
----    let os = (/1)`map`[0..1]
----    xuv <- getXUV>>=return . (`map`os)
----    let xuvKet = (Ket (Just os) [1/sqrt 2,i 1/sqrt 2])
----    --normalise $ Ket (Just os) $ map fromReal xuv
----        xuvRho = xuvKet|><|xuvKet
----        matBy2 = Operator (Just os) [ Ket (Just os) [0.5,0.0]
----                                    , Ket (Just os) [0.0,0.5]
----                                    ]
----    liftIO$putStrLn""
----    liftIO$print $ trace  matBy2
----    liftIO$print $ purity matBy2
----    liftIO$print $ concurrence matBy2
----    liftIO$putStrLn""
----    liftIO$print $ trace  xuvRho
----    liftIO$print $ purity xuvRho
----    liftIO$print $ concurrence xuvRho
----
----
----    getCDictFilePath "outFileRho1ph">>=liftIO.print
