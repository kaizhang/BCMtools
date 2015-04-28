module BCMtools.View
    ( view
    ) where

import qualified Data.ByteString.Lazy as L

import BCMtools.Types
import BCM (ContactMap, _matrix, createContactMap, saveContactMap, closeContactMap, openContactMap)
import BCM.IOMatrix (DMatrix, MCSR, DSMatrix, MMatrix, MSMatrix)
import BCM.Matrix.Instances

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

view :: FilePath -> FilePath -> ViewOptions -> IO ()
view input output opt = do
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
    drawopt = def { _range = lo

