{-# LANGUAGE FlexibleContexts #-}
module BCMtools.View
    ( view
    , viewOptions
    ) where

import qualified Data.ByteString as B
import Data.Serialize (runGet, getWord32le)
import System.IO
import Options.Applicative
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as Bin
import Control.Monad.Trans.Resource (runResourceT)
import Data.Default.Class (def)

import BCMtools.Types
import BCM (ContactMap, _matrix, closeContactMap, openContactMap)
import BCM.IOMatrix (DMatrix, MCSR, DSMatrix, MMatrix, MSMatrix)
import BCM.Matrix.Instances
import BCM.Visualize

viewOptions :: Parser Command 
viewOptions = fmap View $ ViewOptions
          <$> fmap f (strOption
                ( long "range"
               <> short 'r'
               <> metavar "Heatmap range" ))
          <*> switch
                ( long "memory"
               <> help "store matrix in memory" )
  where
    f x = let a = read $ takeWhile (/='-') x
              b = read $ tail $ dropWhile (/='-') x
          in (a, b)

view :: FilePath -> FilePath -> ViewOptions -> IO ()
view input output opt = do
    Right magic <- withFile input ReadMode $ \h -> do
        hSeek h AbsoluteSeek 4
        Right p <- fmap fromIntegral . runGet getWord32le <$> B.hGet h 4
        hSeek h AbsoluteSeek p
        runGet getWord32le <$> B.hGet h 4

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
    draw x = runResourceT $ drawMatrix (_matrix x) drawopt $$ Bin.sinkFile output
    drawopt = def { _range = _valueRange opt }
{-# INLINE view #-}

