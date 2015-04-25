{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
-- most of the codes in this file are directly copied from JuicyPixel

module BCM.Visualize.Internal where

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
import Control.Monad.Trans (lift)
import Data.Colour
import Data.Colour.SRGB

import Data.Conduit.Zlib as Z

import Data.Conduit
import qualified Data.Conduit.List as CL

import BCM.Visualize.Internal.Types

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
toPngData = yield . encode . prepareIDatChunk . L.pack . (0:)

toPngData' :: Conduit [Word8] IO B.ByteString
toPngData' = CL.map (B.pack . (0:)) $= Z.compress 5 Z.defaultWindowBits

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

