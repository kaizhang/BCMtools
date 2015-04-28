{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Options.Applicative
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get
import qualified Data.Conduit.Binary as Bin
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as M
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Default.Class (def)
import System.IO
import Data.ByteString.Lex.Double

import BCM (ContactMap, _matrix, createContactMap, saveContactMap, closeContactMap, openContactMap)
import BCM.IOMatrix (DMatrix, MCSR, DSMatrix, MMatrix, MSMatrix)
import BCM.Matrix.Instances
import BCM.Visualize

data BCMtoolsOptions = BCMtoolsOptions
    { _input :: FilePath
    , _output :: FilePath
    , _command :: Command
    }

globalOptions :: Parser Command -> Parser BCMtoolsOptions
globalOptions cmd = BCMtoolsOptions 
             <$> argument str (metavar "INPUT") 
             <*> strOption
                   ( long "output"
                  <> short 'o'
                  <> metavar "OUTPUT" )
             <*> cmd

data Command = Convert ConvertOptions
             | View ViewOptions

data ConvertOptions = ConvertOptions
    { _genome :: String    -- ^ genome
    , _rownames :: [String]  -- ^ row chrs
    , _colnames :: [String]  -- ^ column chrs
    , _resolution :: Int   -- ^ resolution
    , _sparse :: Bool
    , _symmetric :: Bool
    } deriving (Show)
   
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
             <*> option auto
                   ( long "resolution"
                  <> short 's'
                  <> metavar "RESOLUTION" )
             <*> switch
                   ( long "sparse"
                  <> help "whether to use sparse encoding" )
             <*> switch
                   ( long "symmetric"
                  <> help "whether to use symmetric encoding" )

data ViewOptions = ViewOptions
    { _lo :: Double
    , _hi :: Double
    , _inMem :: Bool
    }

viewOptions :: Parser Command 
viewOptions = fmap View $ ViewOptions
          <$> option auto
                ( short 'l'
               <> metavar "Lower bound" )
          <*> option auto
                ( short 'h'
               <> metavar "Upper bound" )
          <*> switch
                ( long "memory"
               <> help "store matrix in memory" )

bcmtoolsOptions :: Parser BCMtoolsOptions
bcmtoolsOptions = subparser
    ( command "convert" ( info (globalOptions convertOptions)
                               (progDesc "file conversion") )
   <> command "view" ( info (globalOptions viewOptions)
                            (progDesc "view file") )
    )

runBCMtools :: BCMtoolsOptions -> IO () 
runBCMtools (BCMtoolsOptions input output (Convert opt)) = do
    len <- runResourceT $ Bin.sourceFile input $= Bin.lines $$ CL.fold (\i _ -> i+1) 0
    header <- B.split '\t' <$> withFile input ReadMode B.hGetLine
    let (source,n) = case header of
            [f1,f2] -> (Bin.sourceFile input $= Bin.lines $= field2 f1 f2, len-1)
            _ -> undefined
        runner x = runResourceT $ source $$ createContactMap output rows cols (_resolution opt) (Just x)

    case () of
        _ | _sparse opt && _symmetric opt -> do
              cm <- runner n :: IO (ContactMap MCSR)
              saveContactMap cm
              closeContactMap cm
          | _sparse opt -> do
              cm <- runner n :: IO (ContactMap MCSR)
              saveContactMap cm
              closeContactMap cm
          | _symmetric opt -> do
              cm <- runner n :: IO (ContactMap DSMatrix)
              saveContactMap cm
              closeContactMap cm
          | otherwise -> do
              cm <- runner n :: IO (ContactMap DMatrix)
              saveContactMap cm
              closeContactMap cm
  where
    genome = case _genome opt of
        "hg19" -> hg19
        _ -> undefined
    rows = map (\x -> (B.pack x, M.lookupDefault undefined x genome)) $ _rownames opt
    cols = map (\x -> (B.pack x, M.lookupDefault undefined x genome)) $ _colnames opt
    field2 chr1 chr2 = do
        _ <- await
        CL.map f
      where
        f l = let [x1,x2,v] = B.split '\t' l
              in (((chr1, readInt x1), (chr2, readInt x2)), readDouble' v)
runBCMtools (BCMtoolsOptions input output (View opt)) = do
    magic <- withFile input ReadMode $ \h -> do
        hSeek h AbsoluteSeek 4
        p <- fromIntegral . runGet getWord32le <$> L.hGet h 4
        hSeek h AbsoluteSeek p
        runGet getWord32le <$> L.hGet h 4

    case () of
        _ | magic == d_matrix_magic -> do
                hPutStrLn stderr "Format: Dense matrix"
                if _inMem opt
                   then do
                       cm <- openContactMap input :: IO (ContactMap MMatrix)
                       draw cm
                       closeContactMap cm
                   else do
                       cm <- openContactMap input :: IO (ContactMap DMatrix)
                       draw cm
                       closeContactMap cm
          | magic == ds_matrix_magic -> do
                hPutStrLn stderr "Format: Dense symmetric matrix"
                if _inMem opt
                   then do
                       cm <- openContactMap input :: IO (ContactMap MSMatrix)
                       draw cm
                       closeContactMap cm
                   else do
                       cm <- openContactMap input :: IO (ContactMap DSMatrix)
                       draw cm
                       closeContactMap cm
          | magic == sp_matrix_magic -> do
                hPutStrLn stderr "Format: Sparse matrix"
                if _inMem opt
                   then do
                       cm <- openContactMap input :: IO (ContactMap MCSR)
                       draw cm
                       closeContactMap cm
                   else do
                       cm <- openContactMap input :: IO (ContactMap MCSR)
                       draw cm
                       closeContactMap cm
          | otherwise -> error "unknown format"
  where
    draw x = runResourceT $ drawMatrix (_matrix x) def $= CL.map L.toStrict $$ Bin.sinkFile output


main :: IO ()
main = execParser opts >>= runBCMtools
  where
    opts = info (helper <*> bcmtoolsOptions)
         ( fullDesc
        <> progDesc ""
        <> header "" )


hg19 :: M.HashMap String Int
hg19 = M.fromList [("chr1", 249250621)]

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

readDouble' :: B.ByteString -> Double
readDouble' = fst . fromJust . readDouble
