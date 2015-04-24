{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module HiC where

import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import Data.Word
import Data.Binary.Put
import Data.Binary.Get
import Data.Conduit
import qualified Data.Conduit.List as CL
import System.IO
import Data.List (foldl')

import qualified HiC.DiskMatrix as DM
import qualified HiC.IOMatrix as IOM

-- contact map binary format
-- 4 bytes magic + 4 bytes Int (step) + 8 bytes Int (start matrix) + 1 bytes (reserve) + chroms + matrix

data ContactMap mat = ContactMap
    { _rowLabels :: M.HashMap B.ByteString (Int, Int)
    , _colLabels :: M.HashMap B.ByteString (Int, Int)
    , _resolution :: Int
    , _matrix :: mat Double
    }

contact_map_magic :: Word32
contact_map_magic = 0x9921ABF0

toContactMap :: (IOM.IOMatrix mat Double, MonadIO io)
             => FilePath
             -> [(B.ByteString, Int)]
             -> [(B.ByteString, Int)]
             -> Int
             -> Sink (((B.ByteString, Int), (B.ByteString, Int)), Double) io (ContactMap mat)
toContactMap fl rowChr colChr res = do
    -- write header to file
    h <- liftIO $ do
        handle <- openFile fl ReadWriteMode
        L.hPut handle $ runPut $ putWord32le contact_map_magic
        L.hPut handle $ runPut $ putWord32le $ fromIntegral res
        DM.hWrite1 handle offset
        L.hPut handle $ runPut $ putWord8 0
        L.hPut handle rows
        L.hPut handle cols
        return handle

    let source  = CL.map $ \( ((chr1,i), (chr2,j)), v ) ->
            let (p1, s1) = M.lookupDefault undefined chr1 rLab
                (p2, s2) = M.lookupDefault undefined chr2 cLab
                i' = i `div` res + p1
                j' = j `div` res + p2
            in ((i',j'), v)

    m <- source $= IOM.hCreateMatrix h (fromIntegral offset) (r,c)

    return $ ContactMap rLab cLab res m
  where
    r = foldl' (\acc (_,x) -> acc + (x - 1) `div` res + 1)  0 rowChr
    c = foldl' (\acc (_,x) -> acc + (x - 1) `div` res + 1)  0 colChr
    rLab = mkLabels rowChr res
    cLab = mkLabels colChr res
    encodeLab xs = L.intercalate "" (map (\(chr, s) -> L.fromStrict chr `L.append` "\0" `L.append` DM.toByteString s) xs) `L.append` "\0"
    rows = encodeLab rowChr
    cols = encodeLab colChr
    nrow = fromIntegral $ L.length rows
    ncol = fromIntegral $ L.length cols
    offset = 4 + 4 + 8 + 1 + nrow + ncol :: Int


-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

mkLabels :: [(B.ByteString, Int)] -> Int -> M.HashMap B.ByteString (Int, Int)
mkLabels xs step = M.fromList $ foldr f [] xs
  where
    f (chr, size) [] = [(chr, (0, size))]
    f (chr, size) acc@((_, (a, b)) : _) = (chr, (a + ((b - 1) `div` step + 1), size)) : acc
{-# INLINE mkLabels #-}
