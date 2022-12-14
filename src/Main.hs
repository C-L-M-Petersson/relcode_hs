import           Control.Monad
import           Maths.HilbertSpace
--import           Maths.HilbertSpace.State
import           Maths.HilbertSpace.DensityMatrix
import           Maths.Interpolate
import           Maths.QuantumNumbers

import           QState
import           QState.Light
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
    let os = (/1)`map`[0..1]
    xuv <- getXUV>>=return . (`map`os)
    let xuvKet = (Ket (Just os) [1/sqrt 2,i 1/sqrt 2])
    --normalise $ Ket (Just os) $ map fromReal xuv
        xuvRho = xuvKet|><|xuvKet
        matBy2 = Operator (Just os) [ Ket (Just os) [0.5,0.0]
                                    , Ket (Just os) [0.0,0.5]
                                    ]
    liftIO$putStrLn""
    liftIO$print $ trace  matBy2
    liftIO$print $ trace (matBy2*matBy2)
    liftIO$print $ trace (matBy2^2)

    liftIO$putStrLn""
    liftIO$print $ trace  xuvRho
    liftIO$print $ trace (xuvRho*xuvRho)
    liftIO$print $ trace (xuvRho^2)

    --liftIO$putStrLn""
    --liftIO$putStrLn""
    --liftIO$print $ xuvRho
    --liftIO$putStrLn""
    --liftIO$putStrLn""
    --liftIO$print $ xuvRho^2
    --liftIO$putStrLn""
    --liftIO$putStrLn""
    --liftIO$print $ matBy2
    --liftIO$putStrLn""
    --liftIO$putStrLn""
    --liftIO$print $ matBy2^2

    let os = (/1)`map`[0..2]
    xuv <- getXUV>>=return . (`map`os)
    let xuvKet = normalise $ Ket (Just os) $ map fromReal xuv
        xuvRho = xuvKet|><|xuvKet
    liftIO$putStrLn"A:"

    liftIO$print $ trace  xuvRho
    liftIO$print $ trace (xuvRho*xuvRho)
    --liftIO$print $ norm xuvRho
    --liftIO$print $ norm (xuvRho*xuvRho)

    let os = (/10)`map`[0..20]
    xuv <- getXUV>>=return . (`map`os)
    let xuvKet = normalise $ Ket (Just os) $ map fromReal xuv
        xuvRho = xuvKet|><|xuvKet
    liftIO$putStrLn"B:"

    liftIO$print $ trace  xuvRho
    liftIO$print $ trace (xuvRho*xuvRho)
    --liftIO$print $ norm xuvRho
    --liftIO$print $ norm (xuvRho*xuvRho)

    let os = (/100)`map`[0..200]
    xuv <- getXUV>>=return . (`map`os)
    let xuvKet = normalise $ Ket (Just os) $ map fromReal xuv
        xuvRho = xuvKet|><|xuvKet
        --mat2   = Operator (Just os) (replicate 10 0.5)
    liftIO$putStrLn"C:"

    liftIO$print $ trace  xuvRho
    liftIO$print $ trace (xuvRho*xuvRho)
    --liftIO$print $ norm xuvRho
    --liftIO$print $ norm (xuvRho*xuvRho)

--prot :: Operator ->
diagDet = product . diag
    --liftIO$print os

    --liftIO$print . norm     $           xuvKet
    --liftIO$print . norm     $ normalise xuvKet

    --liftIO$print . sum . map (((delta xuvKet*)) . (**2) . absVal) . ketElems $ normalise xuvKet

    --liftIO$print . (fromReal (delta xuvKet)*) . sum . ketElems $           xuvKet
    --liftIO$print . (fromReal (delta xuvKet)*) . sum . ketElems $ normalise xuvKet
    --liftIO$print . (          delta xuvKet *)       . norm     $ normalise xuvKet


    ----liftIO$print [kappas0,ns0,kappas1]
    ----getMatElems kappas0 ns0 kappas1>>=liftIO.print
    ----k <- getExcitedState kappas0 ns0 kappas1 -- >>=liftIO.print . fromState
    --k <- getExcitedState kappas0 ns0 kappas1 -- >>=liftIO.print . fromState
    ----let k' = interpolateKet (map (/1000) [1000..1080]) k
    ----let k' = interpolateKet (map ((+0.05) . (/1)) [0..10]) k
    --let k' = interpolateKet (map ((+0.05) . (/2)) [0..10]) k
    --liftIO $ print k'
    --liftIO $ putStrLn "\nA:"

    --liftIO . mapM_ print . diag   $  fromState k'
    --liftIO $ putStrLn "\nB:"
    --liftIO . mapM_ print . diag   $ (fromState k')^2
    --liftIO $ putStrLn "\nC:"
    --liftIO . print . map (^2) . diag   $  fromState k'
    --liftIO . print . map (^2) . diag   $ (fromState k')^2
    --liftIO $ putStrLn "\nD:"
    --liftIO . print . trace  $ fromState k'
    --liftIO . print . purity $ fromState k'

    ----liftIO . print $ fromState k'
    --return()

--printKet :: [Double] -> [Scalar] -> QState()
--printKet (e:es) (v:vs) = liftIO (putStrLn (show e++" "++show (absVal v)++" "++show (phase v)))>>printKet es vs
--printKet  _      _     = return()
