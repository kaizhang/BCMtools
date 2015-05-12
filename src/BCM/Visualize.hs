{-# LANGUAGE FlexibleContexts #-}
module BCM.Visualize
    ( DrawOpt(..)
    , reds
    , blueRed
    , drawMatrix
    , drawMatrixBy
    ) where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.Serialize (encode)
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)

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

drawMatrix :: (MonadIO io, IOM.IOMatrix mat t Double) => mat t Double -> DrawOpt -> Source io B.ByteString
drawMatrix mat = drawMatrixBy IOM.unsafeTakeRowM w h mat
  where
    (w,h) = IOM.dim mat
{-# INLINE drawMatrix #-}

drawMatrixBy :: (MonadIO io, G.Vector v Double, G.Vector v Word8)
             => (mat -> Int -> IO (v Double))  -- ^ row extraction function
             -> Int                    -- ^ width
             -> Int                    -- ^ height
             -> mat                    -- ^ matrix
             -> DrawOpt                -- ^ options
             -> Source io B.ByteString
drawMatrixBy fn w h mat opt = do
    yield pngSignature
    yield $ encode header
    yield . encode . preparePalette . coloursToPalette . _palette $ opt

    cs <- liftIO $ loop mat 0 $= toPngData $$ CL.consume
    yield $ encode $ prepareIDatChunk $ B.concat cs

    yield $ encode endChunk
  where
    loop m i
      | i < h = do
          row <- liftIO $ fn m i
          yield $ G.toList $ G.map drawPixel row
          loop m (i+1)
      | otherwise = return ()

    drawPixel x | x <= lo = 0
                | x >= hi = fromIntegral $ n - 1
                | otherwise = truncate $ (x - lo) / step
    (lo,hi) = _range opt
    step = (hi - lo) / fromIntegral n
    n = length $ _palette opt
    header = preparePngHeader w h PngIndexedColor 8
{-# INLINE drawMatrixBy #-}

interpolate :: Int -> Colour Double -> Colour Double -> [Colour Double]
interpolate n c1 c2 = loop 1
  where
    loop i | i > n = []
           | otherwise = blend (fromIntegral i * step) c2 c1 : loop (i+1)
    step = 1 / fromIntegral (n+1)
{-# INLINE interpolate #-}
