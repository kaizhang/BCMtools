{-# LANGUAGE FlexibleContexts #-}
module BCMtools.View
    ( view
    , viewOptions
    ) where

import Data.Monoid ((<>))
import qualified Data.ByteString as B
import Data.Serialize (runGet, getWord32le)
import System.IO
import Options.Applicative
import Data.Conduit
import qualified Data.Conduit.Binary as Bin
import Control.Monad.Trans.Resource (runResourceT)
import Data.Default.Class (def)

import BCMtools.Types
import BCM (ContactMap, _matrix, closeContactMap, openContactMap)
import BCM.IOMatrix (DMatrix, MCSR, DSMatrix, MMatrix, MSMatrix)
import BCM.Visualize
import BCM.Binary (ds_matrix_magic, d_matrix_magic, sp_matrix_magic)

viewOptions :: Parser Command
viewOptions = fmap View $ ViewOptions
    <$> option (maybeReader (Just . f '-'))
        ( long "range"
       <> short 'r'
       <> metavar "Heatmap range" )
    <*> (optional . option (maybeReader (Just . f ',')))
        ( long "from-to"
       <> metavar "start and end index, example: --from-to 2,100" )
  where
    f splitter x = (a, b)
      where
        a = read $ takeWhile (/= splitter) x
        b = read $ tail $ dropWhile (/= splitter) x

view :: FilePath -> FilePath -> Bool -> ViewOptions -> IO ()
view input output onDisk opt = do
    Right magic <- withFile input ReadMode $ \h -> do
        hSeek h AbsoluteSeek 4
        Right p <- fmap fromIntegral . runGet getWord32le <$> B.hGet h 4
        hSeek h AbsoluteSeek p
        runGet getWord32le <$> B.hGet h 4

    case () of
        _ | magic == d_matrix_magic -> do
                hPutStrLn stderr "Format: Dense matrix"
                if onDisk
                   then openWith (openContactMap input :: IO (ContactMap DMatrix))
                   else openWith (openContactMap input :: IO (ContactMap MMatrix))
          | magic == ds_matrix_magic -> do
                hPutStrLn stderr "Format: Dense symmetric matrix"
                if onDisk
                   then openWith (openContactMap input :: IO (ContactMap DSMatrix))
                   else openWith (openContactMap input :: IO (ContactMap MSMatrix))
        _ | magic == sp_matrix_magic -> do
                hPutStrLn stderr "Format: Sparse matrix"
                if onDisk
                   then openWith (openContactMap input :: IO (ContactMap MCSR))
                   else openWith (openContactMap input :: IO (ContactMap MCSR))
          | otherwise -> error "unknown format"
  where
    openWith fn = do cm <- fn
                     draw cm
                     closeContactMap cm
    draw x = runResourceT $ drawMatrix (_matrix x) drawopt $$ Bin.sinkFile output
    drawopt = def { _range = _valueRange opt, _fromTo = _from_to opt }
{-# INLINE view #-}
