module QState.Internal.Configure where

import           Data.Char
import           Data.Composition
import           Data.Functor
import           Data.List
import qualified Data.Map           as M
import           Data.Maybe
import           Data.Tuple.HT

import           Safe
import           System.Environment

import           Text.Printf


newtype CDict = CDict { cDictMap :: M.Map String String }


instance Show CDict where
    show (CDict cDictMap) = unlines $ "CDict:":map showKV (M.toList cDictMap)
        where
            showKV :: (String,String) -> String
            showKV (k,v) = pad k++" = "++v

            pad :: String -> String
            pad = printf ("    % "++show padLen++"s")

            padLen :: Int
            padLen = maximum . map (length . fst) $ M.toList cDictMap

loadCDict :: IO CDict
loadCDict = CDict . M.fromList <$> ((++)<$>getCDictFileKVs<*>getArgKVs)
    where
        getArgKVs :: IO [(String,String)]
        getArgKVs = toKeyVals . args<$>getArgs
            where
                toKeyVals :: [String] -> [(String,String)]
                toKeyVals (('-':'-':x):xs)
                    | null xs                = [(x,"")]
                    | take 2 (head xs)=="--" =  (x,"")     :toKeyVals xs
                    | otherwise              =  (x,head xs):toKeyVals (tail xs)
                toKeyVals (x:xs)             =              toKeyVals xs
                toKeyVals []                 = []

                args :: [String] ->  [String]
                args []           = []
                args (x:xs)
                    | head x=='"' = x':xs'
                    | otherwise   = x :args xs
                    where
                        (x',xs') = (\(xs,y:ys) -> (unwords $ x:xs++[y],ys))
                                 $ break ((=='"') . last) xs


        getCDictFileKVs :: IO [(String,String)]
        getCDictFileKVs = (cDictFp>>=readFile)<&>parseCDictFile

        cDictFp :: IO FilePath
        cDictFp = headDef defCDictFp . tailSafe . dropWhile (/="--cfg")<$>getArgs
            where defCDictFp = "kraken.cfg"

        parseCDictFile :: FilePath -> [(String,String)]
        parseCDictFile = map (mapPair (trim,trim . tail) . break (=='='))
                     . filter (elem '=') . map removeComments . lines

        removeComments :: String -> String
        removeComments            ""  = ""
        removeComments ('\\':'\\':xs) = '\\':removeComments xs
        removeComments ('\\':'#' :xs) = '#' :removeComments xs
        removeComments (     '#' :xs) = ""
        removeComments (      x  :xs) =  x  :removeComments xs

        trim :: String -> String
        trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace



insertCDict :: String -> String -> CDict -> CDict
insertCDict k v = CDict . M.insert k v . cDictMap



cDictReadOption :: Read a => String -> CDict -> a
cDictReadOption = read.:cDictOption

cDictReadOptionSafe :: Read a => String -> CDict -> Maybe a
cDictReadOptionSafe = (<$>)read.:cDictOptionSafe

cDictOption :: String -> CDict -> String
cDictOption k = fromMaybe (error $ "option "++k++" not passed") . cDictOptionSafe k

cDictOptionSafe :: String -> CDict -> Maybe String
cDictOptionSafe k = M.lookup k . cDictMap
