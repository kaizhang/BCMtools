{-# LANGUAGE FlexibleInstances #-}
module BCM.Binary where

import Control.Applicative ((<$>))
import Foreign
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as S

import qualified Data.Matrix.Dense.Generic as DM
import qualified Data.Matrix.Symmetric as DS
import qualified Data.Matrix.Sparse.Generic as SM

class Swappable a where
    byteSwap :: a -> a

instance Swappable Double where
    byteSwap x = unsafePerformIO $ with x $ \ptr -> do
        word64 <- byteSwap64 <$> peek (castPtr ptr)
        poke (castPtr ptr) word64
        peek ptr 
    {-# INLINE byteSwap #-}

instance Swappable Int64 where
    byteSwap = fromIntegral . byteSwap64 . fromIntegral
    {-# INLINE byteSwap #-}

class BinaryData a where
    hGetData :: Handle -> Bool -> IO a
    hPutData :: Handle -> a -> IO ()
    size :: a -> Int

instance BinaryData Int64 where
    hGetData hdl = fmap fromIntegral . (hGetData hdl :: Bool -> IO Word64)
    hPutData hdl = (hPutData hdl :: Word64 -> IO ()). fromIntegral
    size _ = 8
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
    size _ = 8
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
        vec <- hGetDoubleVector hdl (r*c)
        if byteSwapped
           then return $ DM.Matrix r c c 0 $ G.map byteSwap $ G.convert vec
           else return $ DM.Matrix r c c 0 $ G.convert vec
    size = undefined
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
        vec <- hGetDoubleVector hdl len
        if byteSwapped
           then return $ DS.SymMatrix n $ G.map byteSwap $ G.convert vec
           else return $ DS.SymMatrix n $ G.convert vec
    size = undefined
    {-# INLINE hGetData #-}
    {-# INLINE hPutData #-}

sp_matrix_magic :: Word32
sp_matrix_magic = 0x177BFFA0

instance BinaryData (SM.CSR U.Vector Double) where
    hPutData hdl (SM.CSR r c vec ci rp) = do
        hPutData hdl sp_matrix_magic
        hPutData hdl (fromIntegral r :: Int64)
        hPutData hdl (fromIntegral c :: Int64)
        hPutData hdl (fromIntegral n :: Int64)
        G.mapM_ (hPutData hdl) vec
        G.mapM_ (hPutData hdl . (fromIntegral :: Int -> Int64)) ci
        G.mapM_ (hPutData hdl . (fromIntegral :: Int -> Int64)) rp
      where
        n = G.length vec

    hGetData hdl _ = do
        magic <- hGetData hdl False
        let byteSwapped | magic == sp_matrix_magic = False
                        | byteSwap32 magic == sp_matrix_magic = True
                        | otherwise = error "Read matrix fail: wrong signature"
        r <- fromIntegral <$> (hGetData hdl byteSwapped :: IO Int64)
        c <- fromIntegral <$> (hGetData hdl byteSwapped :: IO Int64)
        n <- fromIntegral <$> (hGetData hdl byteSwapped :: IO Int64)
        vec <- G.convert <$> hGetDoubleVector hdl n
        ci <- G.convert <$> hGetIntVector hdl n
        rp <- G.convert <$> hGetIntVector hdl c
        if byteSwapped
           then return $ SM.CSR r c (G.map byteSwap vec)
                                    (G.map (fromIntegral . (byteSwap :: Int64 -> Int64) . fromIntegral) ci)
                                    (G.map (fromIntegral . (byteSwap :: Int64 -> Int64) . fromIntegral) rp)
           else return $ SM.CSR r c vec ci rp
    {-# INLINE hGetData #-}
    {-# INLINE hPutData #-}

    size (SM.CSR _ _ vec ci rp) = 28 + 8 * (G.length vec + G.length ci + G.length rp)


------------------------------------------------------------------------------

hGetIntVector :: Handle -> Int -> IO (S.Vector Int)
hGetIntVector hdl n = do
    ptr <- mallocBytes (n*8)
    hGetBuf hdl ptr (n*8)
    ptr' <- newForeignPtr_ ptr
    return $ S.unsafeFromForeignPtr0 ptr' n
{-# INLINE hGetIntVector #-}

hGetDoubleVector :: Handle -> Int -> IO (S.Vector Double)
hGetDoubleVector hdl n = do
    ptr <- mallocBytes (n*8)
    hGetBuf hdl ptr (n*8)
    ptr' <- newForeignPtr_ ptr
    return $ S.unsafeFromForeignPtr0 ptr' n
{-# INLINE hGetDoubleVector #-}
