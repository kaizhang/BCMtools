{-# LANGUAGE FlexibleInstances #-}
module BCM.Binary where

import Control.Applicative ((<$>))
import Foreign
import System.IO

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as S

import qualified Data.Matrix.Dense.Generic as DM
import qualified Data.Matrix.Symmetric as DS
import qualified Data.Matrix.Sparse.Generic as SM

class BinaryData a where
    hGetData :: Handle -> Bool -> IO a
    hPutData :: Handle -> a -> IO ()

instance BinaryData Int64 where
    hGetData hdl = fmap fromIntegral . (hGetData hdl :: Bool -> IO Word64)
    hPutData hdl = (hPutData hdl :: Word64 -> IO ()). fromIntegral
    {-# INLINE hGetData #-}
    {-# INLINE hPutData #-}

instance BinaryData Double where
    hGetData hdl byteSwapped = alloca $ \ptr -> do
        hGetBuf hdl ptr 8
        word64 <- if byteSwapped
                     then byteSwap64 <$> peek ptr
                     else peek ptr
        poke ptr word64
        peek $ castPtr ptr
    hPutData hdl x = with x $ \ptr -> hPutBuf hdl ptr 8
    {-# INLINE hGetData #-}
    {-# INLINE hPutData #-}

instance BinaryData Word64 where
    hGetData hdl byteSwapped = alloca $ \ptr -> do
        hGetBuf hdl ptr 8
        if byteSwapped
           then byteSwap64 <$> peek ptr
           else peek ptr
    hPutData hdl x = with x $ \ptr -> hPutBuf hdl ptr 8
    {-# INLINE hGetData #-}
    {-# INLINE hPutData #-}

instance BinaryData Word32 where
    hGetData hdl byteSwapped = alloca $ \ptr -> do
        hGetBuf hdl ptr 4
        if byteSwapped
           then byteSwap32 <$> peek ptr
           else peek ptr
    hPutData hdl x = with x $ \ptr -> hPutBuf hdl ptr 4
    {-# INLINE hGetData #-}
    {-# INLINE hPutData #-}

d_matrix_magic :: Word32
d_matrix_magic = 0x22D20B77

instance BinaryData (DM.Matrix U.Vector Double) where
    hPutData hdl (DM.Matrix r c _ _ vec) = do
        hPutData hdl d_matrix_magic
        hPutData hdl (fromIntegral r :: Int64)
        hPutData hdl (fromIntegral c :: Int64)
        G.mapM_ (hPutData hdl) vec

    hGetData hdl _ = do
        magic <- hGetData hdl False
        let byteSwapped | magic == d_matrix_magic = False
                        | byteSwap32 magic == d_matrix_magic = True
                        | otherwise = error "Read matrix fail: wrong signature"
        r <- fromIntegral <$> (hGetData hdl byteSwapped :: IO Int64)
        c <- fromIntegral <$> (hGetData hdl byteSwapped :: IO Int64)
        vec <- G.replicateM (r*c) $ hGetData hdl byteSwapped
        return $ DM.Matrix r c c 0 vec
    {-# INLINE hGetData #-}
    {-# INLINE hPutData #-}

ds_matrix_magic :: Word32
ds_matrix_magic = 0x33D31A66

instance BinaryData (DS.SymMatrix U.Vector Double) where
    hPutData hdl (DS.SymMatrix n vec) = do
        hPutData hdl ds_matrix_magic
        hPutData hdl (fromIntegral n :: Int64)
        G.mapM_ (hPutData hdl) vec

    hGetData hdl _ = do
        magic <- hGetData hdl False
        let byteSwapped | magic == ds_matrix_magic = False
                        | byteSwap32 magic == ds_matrix_magic = True
                        | otherwise = error "Read matrix fail: wrong signature"
        n <- fromIntegral <$> (hGetData hdl byteSwapped :: IO Int64)
        let len = ((n+1)*n) `shiftR` 1
        vec <- G.replicateM len $ hGetData hdl byteSwapped
        return $ DS.SymMatrix n vec
    {-# INLINE hGetData #-}
    {-# INLINE hPutData #-}

instance BinaryData (SM.CSR U.Vector Double) where
    hPutData = undefined
    hGetData = undefined
    {-# INLINE hGetData #-}
    {-# INLINE hPutData #-}
