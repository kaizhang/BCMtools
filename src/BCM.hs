{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module BCM
    ( ContactMap(..)
    , createContactMap
    , saveContactMap
    , openContactMap
    , closeContactMap
    ) where

import Control.Monad (when, guard)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import Data.Binary.Put
import Data.Binary.Get
import Data.Conduit
import qualified Data.Conduit.List as CL
import System.IO
import Data.List (foldl')
import Data.Word (Word32)
import Text.Printf (printf)

import qualified BCM.DiskMatrix as DM
import qualified BCM.IOMatrix as IOM

-- contact map binary format
-- 4 bytes magic + 4 byte Int (matrix start) + 4 bytes Int (step) + chroms + 1 bytes (reserve) + matrix

data ContactMap m = ContactMap
    { _rowLabels :: M.HashMap B.ByteString (Int, Int)
    , _colLabels :: M.HashMap B.ByteString (Int, Int)
    , _resolution :: Int
    , _matrix :: m
    , _handle :: Handle
    }


contact_map_magic :: Word32
contact_map_magic = 0x9921ABF0

createContactMap :: (IOM.IOMatrix m t Double, MonadIO io, mat ~ m t Double)
                 => FilePath
                 -> [(B.ByteString, Int)]
                 -> [(B.ByteString, Int)]
                 -> Int
                 -> Maybe Int
                 -> Sink (((B.ByteString, Int), (B.ByteString, Int)), Double) io (ContactMap mat)
createContactMap fl rowChr colChr res len = do
    h <- liftIO $ openFile fl ReadWriteMode

    let source  = CL.mapM $ \( ((chr1,i), (chr2,j)), v ) -> do
            let (p1, s1) = M.lookupDefault undefined chr1 rLab
                (p2, s2) = M.lookupDefault undefined chr2 cLab
                i' = i `div` res + p1
                j' = j `div` res + p2
            when (i `mod` res /= 0 || j `mod` res /=0) $
                liftIO $ hPutStrLn stderr $ printf "(%d,%d) is not divisible by %d" i j res
            return ((i',j'), v)


    liftIO $ L.hPut h $ L.replicate (fromIntegral offset) 0

    m <- source $= IOM.hCreateMatrix h (r,c) len

    return $ ContactMap rLab cLab res m h
  where
    r = foldl' (\acc (_,x) -> acc + (x - 1) `div` res + 1)  0 rowChr
    c = foldl' (\acc (_,x) -> acc + (x - 1) `div` res + 1)  0 colChr
    rLab = mkLabels rowChr res
    cLab = mkLabels colChr res
    nByte x = let n1 = foldl' (+) 0 $ map B.length $ fst $ unzip x
                  n2 = 16 * n3
                  n3 = length x
               in n1 + n2 + n3
    offset = 4 + 4 + 4 + nByte rowChr + nByte colChr + 2 + 1

saveContactMap :: (IOM.IOMatrix m t a, mat ~ m t a) => ContactMap mat -> IO ()
saveContactMap (ContactMap rowChr colChr res mat handle) = do
        hSeek handle AbsoluteSeek 0
        L.hPutStr handle . runPut . putWord32le $ contact_map_magic
        L.hPutStr handle . runPut . putWord32le $ offset
        L.hPutStr handle . runPut . putWord32le . fromIntegral $ res
        L.hPutStr handle rowAndcol
        L.hPutStr handle . runPut . putWord8 $ 0
        IOM.hSaveMatrix handle mat
      where
        rowAndcol = L.concat [rows, "\0", cols, "\0"]
        rows = encodeLab . M.toList $ rowChr
        cols = encodeLab . M.toList $ colChr
        encodeLab xs = L.concat $ concatMap (\(chr, (a,b)) ->
            [L.fromStrict chr, "\0", DM.toByteString a, DM.toByteString b]) xs
        offset = fromIntegral $ 4 + 4 + 4 + L.length rowAndcol + 1

openContactMap :: (IOM.IOMatrix m t a, MonadIO io, mat ~ m t a) => FilePath -> io (ContactMap mat)
openContactMap fl = liftIO $ do
    h <- openFile fl ReadWriteMode

    magic <- runGet getWord32le <$> L.hGet h 4
    guard $ magic == contact_map_magic
    _ <- fromIntegral . runGet getWord32le <$> L.hGet h 4
    res <- fromIntegral . runGet getWord32le <$> L.hGet h 4

    rows <- M.fromList <$> getChrs [] h
    cols <- M.fromList <$> getChrs [] h
    _ <- runGet getWord8 <$> L.hGet h 1
    mat <- IOM.hReadMatrix h
    return $ ContactMap rows cols res mat h
  where
    getChrs acc h = do
        chr <- getByteStringNul h
        if B.null chr
           then return acc
           else do
               a <- fromIntegral . runGet getWord64le <$> L.hGet h 8
               b <- fromIntegral . runGet getWord64le <$> L.hGet h 8
               getChrs ((chr, (a, b)) : acc) h
    getByteStringNul h = B.concat <$> go []
      where
        go acc = do
            x <- B.hGet h 1
            case x of
                "\0" -> return acc
                _ -> go $ acc ++ [x]

closeContactMap :: ContactMap mat -> IO ()
closeContactMap cm = hClose $ _handle cm


-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

mkLabels :: [(B.ByteString, Int)] -> Int -> M.HashMap B.ByteString (Int, Int)
mkLabels xs step = M.fromList $ foldr f [] xs
  where
    f (chr, size) [] = [(chr, (0, size))]
    f (chr, size) acc@((_, (a, b)) : _) = (chr, (a + ((b - 1) `div` step + 1), size)) : acc
{-# INLINE mkLabels #-}
