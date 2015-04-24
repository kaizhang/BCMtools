{-# LANGUAGE FlexibleContexts #-}
module HiC.Visualize where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as L
import Data.Binary (encode)
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Data.Conduit
import qualified Data.Vector.Unboxed as U

import HiC.Visualize.Internal
import HiC.Visualize.Internal.Types
import qualified HiC.DiskMatrix as DM

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

drawMatrix :: (MonadIO io, DM.DiskMatrix mat Double) => mat Double -> DrawOpt -> Source io L.ByteString
drawMatrix mat opt = do
    yield pngSignature
    yield $ encode header
    yield . encode . preparePalette . coloursToPalette . _palette $ opt
    loop mat 0
    yield $ encode endChunk
  where
    loop m i
      | i < h = do
          row <- DM.unsafeReadRow m i
          toPngData $ U.toList $ U.map drawPixel row
          loop m (i+1)
      | otherwise = return ()
    drawPixel _ = 60
    {-
    drawPixel x | x <= lo = 0
                | x >= hi = fromIntegral $ n - 1
                | otherwise = truncate $ (x - lo) / step
                -}
    (w,h) = DM.dim mat
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
