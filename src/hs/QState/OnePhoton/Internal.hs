module QState.OnePhoton.Internal
(   omegas
,   eKins
,   amps
,   pCurs
,   phaseF
,   phaseG

,   matElem
) where

import           Control.Monad.Loops

import           Data.Composition
import           Data.List                   (transpose)
import           Data.Function.Flippers

import           Maths.HilbertSpace
import           Maths.QuantumNumbers
import           Maths.WignerSymbol

import           QState.Configure.Internal
import           QState.FilePath.Internal
import           QState.HartreeFock.Internal
import           QState.PertWave.Internal
import           QState.Utility.Parse

import           System.Directory
import           System.IO.Strict as Strict


filePath :: FilePath -> QNum -> QNum -> CDict -> FilePath
filePath file kappa0 n0 cDict = pertDir kappa0 n0 cDict++"/"++file++".dat"

fileExists :: FilePath -> QNum -> QNum -> CDict -> IO Bool
fileExists = doesFileExist.::filePath

fileLines :: FilePath -> QNum -> QNum -> CDict -> IO [String]
fileLines file kappa0 n0 cDict = (lines<$>) . Strict.readFile
                               $ filePath file kappa0 n0 cDict

fileCol :: FilePath -> QNum -> QNum -> Int -> CDict -> IO [String]
fileCol file kappa0 n0 col cDict = map ((!!col) . words)
                                            <$>fileLines file kappa0 n0 cDict

readFileLines :: Read a => FilePath -> QNum -> QNum -> CDict -> IO [a]
readFileLines file kappa0 n0 cDict = map read<$>fileLines file kappa0 n0 cDict

readBreakPointFilesColIndex :: Read a => FilePath -> QNum -> QNum -> Int -> Int
                                                            -> CDict -> IO [a]
readBreakPointFilesColIndex file kappa0 n0 col bPI cDict =
    return . map (read . (!!bPI))
                            =<<mapM       (flip5 fileCol    cDict col n0 kappa0)
                            =<<takeWhileM (flip4 fileExists cDict     n0 kappa0)
                                (map ((file++) . show) ([1..] :: [Int]))

readBreakPointFilesColKappa :: (Num a,Read a) => FilePath -> QNum -> QNum
                                                    -> QNum -> CDict -> IO [a]
readBreakPointFilesColKappa file kappa0 n0 kappa1 cDict
    | bPI<   0  = error "breakPointIndex < 0"
    | bPI>   4  = error "breakPointIndex > 5"
    | col== -1  = (`replicate`0) . length<$>colAll 0
    | bPI==  2  = map read<$>colAll col
    | otherwise = readBreakPointFilesColIndex file kappa0 n0 col bPI cDict
    where
        bPI = cDictReadOption "breakPointIndex" cDict
        col | kappa1==  kappa0-signum kappa0 =  0
            | kappa1== -kappa0               =  1
            | kappa1==  kappa0+signum kappa0 =  2
            | otherwise                      = -1
        colAll col = fileCol (file++"all") kappa0 n0 (col+1) cDict


omegas :: QNum -> QNum -> CDict -> IO [Double]
omegas = readFileLines "omega"

eKins  :: QNum -> QNum -> CDict -> IO [Double]
eKins kappa0 n0 cDict = do
    hfE <- hfEnergy kappa0 n0 cDict
    os  <- omegas   kappa0 n0 cDict

    return $ map (+hfE) os

amps :: QNum -> QNum -> QNum -> CDict -> IO [Double]
amps = readBreakPointFilesColKappa "/amp_"

pCurs :: QNum -> QNum -> QNum -> CDict -> IO [Double]
pCurs = readBreakPointFilesColKappa "/pcur_"

phaseF :: QNum -> QNum -> QNum -> CDict -> IO [Double]
phaseF = readBreakPointFilesColKappa "/phaseF_"

phaseG :: QNum -> QNum -> QNum -> CDict -> IO [Double]
phaseG = readBreakPointFilesColKappa "/phaseG_"


matElem :: QNum -> QNum -> QNum -> QNum -> CDict -> IO [Scalar]
matElem kappa0 n0 kappa1 mJ cDict = map ((*w3j) . product) . transpose
    <$>sequence [ map        fromReal <$>amps   kappa0 n0 kappa1 cDict
                , map (exp . fromImag)<$>phaseF kappa0 n0 kappa1 cDict
                , map (subtractedPhaseFactor kappa1 zEff)
                                      <$>eKins kappa0 n0 cDict
                ]
    where
        zEff = cDictReadOption "zEff" cDict :: Double
        w3j  = wigner3j   j0  1  j1
                        (-mJ) 0  mJ
        j0   = jFromKappa kappa0
        j1   = jFromKappa kappa1
