{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module BCMtools.Convert
    ( convert
    , convertOptions
    ) where

import Data.Monoid ((<>))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lex.Fractional (readSigned, readExponential)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as Bin
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Options.Applicative
import System.IO

import BCMtools.Types
import BCM (ContactMap, createContactMap, saveContactMap, closeContactMap)
import BCM.IOMatrix (DMatrix, MCSR, DSMatrix, MSMatrix, MMatrix)

convertOptions :: Parser Command
convertOptions = fmap Convert $ ConvertOptions
             <$> strOption
                   ( long "genome"
                  <> short 'g'
                  <> metavar "ASSEMBLY"
                  <> help "e.g., hg19, or a file" )
             <*> fmap (splitOn ",") (strOption
                   ( long "rownames"
                  <> short 'r'
                  <> metavar "ROW LABELS" ))
             <*> fmap (splitOn ",") (strOption
                   ( long "colnames"
                  <> short 'c'
                  <> metavar "COLOUMN LABELS" ))
             <*> fmap readInt' (strOption
                   ( long "resolution"
                  <> short 's'
                  <> metavar "RESOLUTION" ))
             <*> switch
                   ( long "sparse"
                  <> help "whether to use sparse encoding" )
             <*> switch
                   ( long "symmetric"
                  <> help "whether to use symmetric encoding" )
  where
    readInt' x = let (Just (i, left)) = B.readInt $ B.pack x
                 in case () of
                     _ | B.null left -> i
                       | left == "k" || left == "K" -> i * 1000
                       | otherwise -> i

convert :: FilePath -> FilePath -> Bool -> ConvertOptions -> IO ()
convert input output onDisk opt = do
    genome <- case _genome opt of
        "hg19" -> return hg19
        fl -> readGenome fl
    inputLength <- runResourceT $ Bin.sourceFile input $=
                                  Bin.lines $$ CL.fold (\i _ -> i+1) 0
    line1 <- B.split '\t' <$> withFile input ReadMode B.hGetLine
    let fn = case line1 of
            [_,_,_] -> field2
            [_,_,_,_,_] -> field5
            _ -> error "Please check your input format"
        runner f = runResourceT $
                   Bin.sourceFile input $=
                   Bin.lines $= f $$
                   createContactMap output rows cols (_resolution opt) (Just inputLength)
        rows = getChrSize genome $ _rownames opt
        cols = getChrSize genome $ _colnames opt

    case () of
        _ | _sparse opt && _symmetric opt -> do
              if onDisk
                 then createMapWith (runner fn :: IO (ContactMap MCSR))
                 else createMapWith (runner fn :: IO (ContactMap MCSR))
          | _sparse opt ->
              if onDisk
                 then createMapWith (runner fn :: IO (ContactMap MCSR))
                 else createMapWith (runner fn :: IO (ContactMap MCSR))
          | _symmetric opt ->
              if onDisk
                 then createMapWith (runner fn :: IO (ContactMap DSMatrix))
                 else createMapWith (runner fn :: IO (ContactMap MSMatrix))
          | otherwise -> do
              if onDisk
                 then createMapWith (runner fn :: IO (ContactMap DMatrix))
                 else createMapWith (runner fn :: IO (ContactMap MMatrix))
  where
    createMapWith run = do cm <- run
                           saveContactMap cm
                           closeContactMap cm
    readGenome x = do
        c <- B.readFile x
        return $ M.fromList $ map ((\[a,b] -> (B.unpack a, readInt b)) . B.words) $ B.lines c
    getChrSize g = map lookup'
      where
        lookup' x = (B.pack x, M.lookupDefault errMsg x g)
          where
            errMsg = error $ "Unknown chromosome: " ++ x
    field2 = do
        _ <- await
        CL.map f
      where
        f l = let [x1,x2,v] = B.split '\t' l
              in (B.pack chr1, readInt x1, B.pack chr2, readInt x2, readDouble v)
        [chr1] = _rownames opt
        [chr2] = _colnames opt
    field5 = CL.map f
      where
        f l = let [x1,x2,x3,x4,x5] = B.split '\t' l
              in (x1, readInt x2, x3, readInt x4, readDouble x5)
{-# INLINE convert #-}

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt
{-# INLINE readInt #-}

readDouble :: B.ByteString -> Double
readDouble = fst . fromJust . readSigned readExponential
{-# INLINE readDouble #-}

hg19 :: M.HashMap String Int
hg19 = M.fromList [ ("chr1", 249250621)
                  , ("chr2", 243199373)
                  , ("chr3", 198022430)
                  , ("chr4", 191154276)
                  , ("chr5", 180915260)
                  , ("chr6", 171115067)
                  , ("chr7", 159138663)
                  , ("chrX", 155270560)
                  , ("chr8", 146364022)
                  , ("chr9", 141213431)
                  , ("chr10", 135534747)
                  , ("chr11", 135006516)
                  , ("chr12", 133851895)
                  , ("chr13", 115169878)
                  , ("chr14", 107349540)
                  , ("chr15", 102531392)
                  , ("chr16", 90354753)
                  , ("chr17", 81195210)
                  , ("chr18", 78077248)
                  , ("chr20", 63025520)
                  , ("chrY", 59373566)
                  , ("chr19", 59128983)
                  , ("chr22", 51304566)
                  , ("chr21", 48129895)
                  ]
