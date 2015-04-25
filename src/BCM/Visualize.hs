{-# LANGUAGE FlexibleContexts #-}
module BCM.Visualize where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as L
import Data.Binary (encode)
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Vector.Unboxed as U

import BCM.Visualize.Internal
import BCM.Visualize.Internal.Types
import qualified BCM.IOMatrix as IOM

data DrawOpt = DrawOpt 
    { _range :: !(Double, Double)
    , _palette :: ![Colour Double]
    }

instance Default DrawOpt where
    def = DrawOpt
        { _range = (0,10)
        , _palette = reds
        }

reds :: [Colour Double]
reds = interpolate 62 white red

blueRed :: [Colour Double]
blueRed = interpolate 30 blue white ++ interpolate 30 white red

drawMatrix :: (MonadIO io, IOM.IOMatrix mat Double) => mat Double -> DrawOpt -> Source io L.ByteString
drawMatrix mat opt = do
    yield pngSignature
    yield $ encode header
    yield . encode . preparePalette . coloursToPalette . _palette $ opt

    cs <- liftIO $ loop mat 0 $= toPngData' $$ CL.consume
    yield $ encode $ prepareIDatChunk $ L.fromChunks cs

    yield $ encode endChunk
  where
    loop m i
      | i < h = do
          row <- IOM.unsafeTakeRowM m i
          yield $ U.toList $ U.map drawPixel row
          loop m (i+1)
      | otherwise = return ()

    drawPixel x | x <= lo = 0
                | x >= hi = fromIntegral $ n - 1
                | otherwise = truncate $ (x - lo) / step
    (w,h) = IOM.dim mat
    (lo,hi) = _range opt
    step = (hi - lo) / fromIntegral n
    n = length $ _palette opt
    header = preparePngHeader w h PngIndexedColor 8

interpolate :: Int -> Colour Double -> Colour Double -> [Colour Double]
interpolate n c1 c2 = loop 1
  where
    loop i | i > n = []
           | otherwise = blend (fromIntegral i * step) c2 c1 : loop (i+1)
    step = 1 / fromIntegral (n+1)
{-# INLINE interpolate #-}
