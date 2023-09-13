module Experiment.CrossSections.OnePhoton where

--import           Control.Monad
import           Control.Monad.Extra
--
--import           Data.Composition
import           Data.List
import           Data.List.Extra
import           Data.Maybe
--
import           Maths.HilbertSpace.Ket
import           Maths.HilbertSpace.Scalar
--import           Maths.HilbertSpace.Operator.DensityMatrix
--import           Maths.HilbertSpace.Evolving.WignerFunction
import           Maths.QuantumNumbers

import           QState
--import           QState.Coherence
import           QState.Configure
import           QState.Energy
import           QState.OnePhoton
import           QState.Output
import           QState.Units.Energy
import           QState.Units.Internal



crossSections1ph :: QState()
crossSections1ph = whenRunCrossSections1ph . join $ crossSections1phForQNums
    <$>getReadOption "kappas0"<*>getReadOption "ns0"<*>getReadOption "kappas1"

crossSections1phForQNums :: [QNum] -> [QNum] -> [QNum] -> QState()
crossSections1phForQNums kappas0 ns0 kappas1 = whenRunCrossSections1ph $ do
    sequence_
        [ ifSaveData saveCrossSections1ph   "CrossSection"
        , ifSaveData saveBranchingRatios1ph "BranchingRatio"
        ]
    where ifSaveData command key = whenM (getReadOption ("save"++key++"1ph"))
                                 $ command kappas0 ns0 kappas1

whenRunCrossSections1ph :: QState() -> QState()
whenRunCrossSections1ph = whenM (getReadOption "runCrossSection1ph")



saveCrossSections1ph :: [QNum] -> [QNum] -> [QNum] -> QState()
saveCrossSections1ph kappas0 ns0 kappas1 = mapM_ ($kappas1)
                        $ zipWith saveCrossSections1phByGroundState kappas0 ns0
    where
        saveCrossSections1phByGroundState :: QNum -> QNum -> [QNum] -> QState()
        saveCrossSections1phByGroundState kappa0 n0 kappas1 = withOptions
                                                    ["kappas0","ns0","kappas1"]
                                          (map show [[kappa0 ],[n0 ], kappas1'])
            $ (`putStrQStateVec`"outFileCrossSection1ph") . transpose
                =<<mapM (getTotalTransitionAmplitudesByGroundState kappa0 n0)
                        ([kappas1']++map (:[]) kappas1')
            where kappas1' = filter (`elem`kappas1) $ reachableKappas kappa0


saveBranchingRatios1ph :: [QNum] -> [QNum] -> [QNum] -> QState()
saveBranchingRatios1ph kappas0 ns0 kappas1 = (`mapM_`groundStateGroups)
                    (\(n0,k0,k0') -> saveBranchingRatio1ph k0 n0 k0' n0 kappas1)
    where
        --groundStateGroups :: [(QNum,QNum,QNum)]
        groundStateGroups = map (\[(n0,k0),(_,k0')] -> (n0,k0,k0'))
                          . filter ((==2) . length)
                          . groupOn (lFromKappa . snd) $ zip ns0 kappas0

saveBranchingRatio1ph :: QNum -> QNum -> QNum -> QNum -> [QNum] -> QState()
saveBranchingRatio1ph kappa0 n0 kappa0' n0' kappas1 = withOptions
                                                    ["kappas0","ns0","kappas1"]
                                          (map show [[kappa0 ],[n0 ], kappas1'])
    $ (`putStrQStateVec`"outFileBranchingRatio1ph") . map (:[])
        =<<getBranchingRatio kappa0 n0 kappa0' n0' kappas1
    where kappas1' = filter (`elem`kappas1) $ reachableKappas kappa0

getBranchingRatio :: QNum -> QNum -> QNum -> QNum -> [QNum] -> QState [Double]
getBranchingRatio kappa0 n0 kappa0' n0' kappas1 = zipWith getRatio
        <$>getTotalTransitionAmplitudesByGroundState kappa0  n0  kappas1
        <*>getTotalTransitionAmplitudesByGroundState kappa0' n0' kappas1
    where getRatio a a'
            | a==0||a'==0 = 0
            | otherwise   = a'**2/a**2



putStrQStateVec :: [[Double]] -> String -> QState()
putStrQStateVec ass fpK = (`showVec`ass)<$>(getEGrid>>=mapM scaleEs)
                                                    >>=putStrQStateFile fpK
    where scaleEs e = (e*) . (toUnitFactor :: EnergyUnit -> Double)
                                                <$>getReadOption "energyUnits"

showVec :: [Double] -> [[Double]] -> String
showVec (e:es) (as:ass) = unwords (show e:map show as++["\n"])++showVec es ass
showVec  _      _       = ""



getTotalTransitionAmplitudesByGroundState :: QNum -> QNum -> [QNum]
                                                            -> QState [Double]
getTotalTransitionAmplitudesByGroundState kappa0 n0 kappas1 =
    map sum . transpose<$>getTransitionAmplitudesByGroundState kappa0 n0 kappas1

getTransitionAmplitudesByGroundState :: QNum -> QNum -> [QNum]
                                                            -> QState [[Double]]
getTransitionAmplitudesByGroundState kappa0 n0 kappas1 = (`mapM`kappas1)
    $ getTransitionAmplitude kappa0 n0

getTransitionAmplitude :: QNum -> QNum -> QNum -> QState [Double]
getTransitionAmplitude kappa0 n0 kappa1 = withOption "pulseType" "None"
    $ getInterpolatedExcitedStateByOmega [kappa0] [n0] [kappa1]
        >>=energyKetToEGrid>>=return . map ((**2) . absVal) . ketElems
