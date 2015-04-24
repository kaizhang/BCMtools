{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
-- most of the codes in this file are directly copied from JuicyPixel

module HiC.Visualize.Internal where

#if !MIN_VERSION_base(4,8,0)
import Foreign.ForeignPtr.Safe( ForeignPtr, castForeignPtr )
#else
import Foreign.ForeignPtr( ForeignPtr, castForeignPtr )
#endif

import Foreign.Storable( Storable, sizeOf )
import Data.Binary (encode)
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Vector.Storable (Vector, unsafeToForeignPtr)
import qualified Data.ByteString.Internal as S
import qualified Data.Vector.Generic as G
import qualified Data.DList as DL
import qualified Codec.Compression.Zlib as Z
import Control.Monad.Trans (lift)
import Data.Colour
import Data.Colour.SRGB

import Data.Conduit

import HiC.Visualize.Internal.Types

preparePngHeader :: Int -> Int -> PngImageType -> Word8 -> PngIHdr
preparePngHeader w h imgType depth = PngIHdr
    { width             = fromIntegral w
    , height            = fromIntegral h
    , bitDepth          = depth
    , colourType        = imgType
    , compressionMethod = 0
    , filterMethod      = 0
    , interlaceMethod   = PngNoInterlace
    }

prepareIDatChunk :: L.ByteString -> PngRawChunk
prepareIDatChunk imgData = PngRawChunk
    { chunkLength = fromIntegral $ L.length imgData
    , chunkType   = iDATSignature
    , chunkCRC    = pngComputeCrc [iDATSignature, imgData]
    , chunkData   = imgData
    }

endChunk :: PngRawChunk
endChunk = PngRawChunk { chunkLength = 0
                       , chunkType = iENDSignature
                       , chunkCRC = pngComputeCrc [iENDSignature]
                       , chunkData = L.empty
                       }

preparePalette :: Palette -> PngRawChunk
preparePalette pal = PngRawChunk
  { chunkLength = fromIntegral $ G.length pal
  , chunkType   = pLTESignature
  , chunkCRC    = pngComputeCrc [pLTESignature, binaryData]
  , chunkData   = binaryData
  }
   where binaryData = L.fromChunks [toByteString pal]

toByteString :: forall a. (Storable a) => Vector a -> B.ByteString
toByteString vec = S.PS (castForeignPtr ptr) offset (len * size)
  where (ptr, offset, len) = unsafeToForeignPtr vec
        size = sizeOf (undefined :: a)

coloursToPalette :: [Colour Double] -> Palette
coloursToPalette = G.fromList . concatMap f
  where
    f c = let RGB r g b = toSRGB24 c
          in [r,g,b]

toPngData :: Monad m => [Word8] -> Source m L.ByteString
toPngData = yield . encode . prepareIDatChunk . L.pack

generatePng :: Monad m => Int -> Int -> Palette -> (Int -> Int -> m Word8) -> Int -> Source m L.ByteString
generatePng w h pal fn bufSize = do
    yield pngSignature
    yield $ encode header
    yield $ encode $ preparePalette pal
    loop DL.empty 0 0 0
    yield $ encode endChunk
  where
    header = preparePngHeader w h PngIndexedColor 8
    zero = B.singleton 0
    loop acc n i j
        | i < w && j < h = if n < bufSize
                              then do
                                  x <- lift $ fn i j
                                  loop (DL.snoc acc $ B.singleton x) (n+1) i (j+1)
                              else do
                                  output
                                  loop DL.empty 0 i (j+1)
        | i < w = loop acc n (i+1) 0
        | otherwise = output
      where
        output = yield . encode . prepareIDatChunk . Z.compress . L.fromChunks . DL.toList $ acc

{-
genericEncodePng :: forall px. (Pixel px, PixelBaseComponent px ~ Word8)
                 => Int -> Int -> Maybe Palette -> PngImageType -> Image px
                 -> L.ByteString
genericEncodePng w h palette imgKind
                 image@(Image { imageWidth = w, imageHeight = h, imageData = arr }) =
  encode PngRawImage { header = hdr
                     , chunks = prependPalette palette [prepareIDatChunk imgEncodedData, endChunk]}
    where hdr = preparePngHeader w h image imgKind 8
          zero = B.singleton 0

          compCount = componentCount (undefined :: px)

          prependPalette Nothing l = l
          prependPalette (Just p) l = preparePalette p : l

          lineSize = compCount * w
          encodeLine line = blitVector arr (line * lineSize) lineSize
          imgEncodedData = Z.compress . L.fromChunks
                        $ concat [[zero, encodeLine line] | line <- [0 .. h - 1]]
                        -}

