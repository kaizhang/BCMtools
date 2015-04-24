module HiC where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import Data.Word
import Data.Binary.Put
import Data.Binary.Get
import Data.Conduit
import qualified Data.Conduit.List as CL
import System.IO

import HiC.DiskMatrix

-- contact map binary format
-- 4 bytes magic + 4 bytes Int + 8 bytes Int + matrix

data ContactMap mat = ContactMap
    { _rowLabels :: M.HashMap B.ByteString Int
    , _colLabels :: M.HashMap B.ByteString Int
    , _resolution :: Int
    , _matrix :: mat Double
    }

contact_map_magic :: Word32
contact_map_magic = 0x9921ABF0

toContactMap :: DiskMatrix mat Double
             => FilePath
             -> [(B.ByteString, Int)]
             -> [(B.ByteString, Int)]
             -> Int
             -> Sink ((B.ByteString, Int, Int), Double) IO (ContactMap mat)
toContactMap fl chroms res = withFile fl WriteMode $ \handle -> do
    L.hPut handle $ runPut $ putWord32le contact_map_magic


-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

mkLabels :: [(B.ByteString, Int)] -> M.HashMap B.ByteString Int
mkLabels xs = foldl' f [] xs
  where
    f [] (chr, size) = (chr, 0, size)

