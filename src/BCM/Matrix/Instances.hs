{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module BCM.Matrix.Instances where

#if !MIN_VERSION_base(4,8,0)
import Foreign.ForeignPtr.Safe( ForeignPtr, castForeignPtr, sizeOf )
#else
import Foreign.ForeignPtr( ForeignPtr, castForeignPtr)
#endif

import Foreign.Storable (sizeOf)
import Control.Monad (guard)
import Control.Applicative ((<$>))
import Data.Bits (shiftR)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Serialize
import Data.Word (Word32)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as S

import qualified Data.Matrix.Dense.Generic as DM
import qualified Data.Matrix.Symmetric as DS
import qualified Data.Matrix.Sparse.Generic as SM

d_matrix_magic :: Word32
d_matrix_magic = 0x22D20B77

instance Serialize (DM.Matrix U.Vector Double) where
    put = putMatrix putFloat64le
    get = getMatrix getFloat64le
    {-# INLINE put #-}
    {-# INLINE get #-}

instance Serialize (DM.Matrix U.Vector Int) where
    put = putMatrix $ putWord64le . fromIntegral
    get = getMatrix $ fromIntegral <$> getWord64le
    {-# INLINE put #-}
    {-# INLINE get #-}

putMatrix :: G.Vector v a => (a -> Put) -> DM.Matrix v a -> Put
putMatrix putElement (DM.Matrix r c _ _ vec) = do
    putWord32le d_matrix_magic
    putWord64le . fromIntegral $ r
    putWord64le . fromIntegral $ c
    G.mapM_ putElement vec
{-# INLINE putMatrix #-}

getMatrix :: G.Vector v a => Get a -> Get (DM.Matrix v a)
getMatrix getElement = do
    m <- getWord32le
    guard $ m == d_matrix_magic
    r <- fromIntegral <$> getWord64le
    c <- fromIntegral <$> getWord64le
    vec <- G.replicateM (r*c) getElement
    return $ DM.Matrix r c c 0 vec
{-# INLINE getMatrix #-}


ds_matrix_magic :: Word32
ds_matrix_magic = 0x33D31A66

instance Serialize (DS.SymMatrix U.Vector Double) where
    put = putSymMatrix putFloat64le
    get = getSymMatrix
    {-# INLINE put #-}
    {-# INLINE get #-}

putSymMatrix :: G.Vector v a => (a -> Put) -> DS.SymMatrix v a -> Put
putSymMatrix putElement (DS.SymMatrix n vec) = do
    putWord32le ds_matrix_magic
    putWord64le . fromIntegral $ n
    G.mapM_ putElement vec
{-# INLINE putSymMatrix #-}

getSymMatrix :: G.Vector v Double => Get (DS.SymMatrix v Double)
getSymMatrix = do
    m <- getWord32le
    guard $ m == ds_matrix_magic
    n <- fromIntegral <$> getWord64le
    let len = ((n+1)*n) `shiftR` 1
    bs <- getByteString (len*8)
    let vec = G.generate len $ \i -> let Right r = runGet getFloat64le . B.take 8 . B.drop (i*8) $ bs
                                     in r
    return $ DS.SymMatrix n vec
{-# INLINE getSymMatrix #-}


sp_matrix_magic :: Word32
sp_matrix_magic = 0x177BFFA0

instance Serialize (SM.CSR U.Vector Double) where
    put = putCSR putFloat64le
    get = getCSR getFloat64le
    {-# INLINE put #-}
    {-# INLINE get #-}

instance Serialize (SM.CSR U.Vector Int) where
    put = putCSR $ putWord64le . fromIntegral
    get = getCSR $ fromIntegral <$> getWord64le
    {-# INLINE put #-}
    {-# INLINE get #-}

putCSR :: G.Vector v a => (a -> Put) -> SM.CSR v a -> Put
putCSR putElement (SM.CSR r c vec ci rp) = do
    putWord32le sp_matrix_magic
    putWord64le . fromIntegral $ r
    putWord64le . fromIntegral $ c
    putWord64le . fromIntegral $ n
    G.mapM_ putElement vec
    G.mapM_ (putWord64le . fromIntegral) ci
    G.mapM_ (putWord64le . fromIntegral) rp
  where
    n = G.length vec
{-# INLINE putCSR #-}

getCSR :: G.Vector v a => Get a -> Get (SM.CSR v a)
getCSR getElement = do
    m <- getWord32le
    guard $ m == sp_matrix_magic
    r <- fromIntegral <$> getWord64le
    c <- fromIntegral <$> getWord64le
    n <- fromIntegral <$> getWord64le
    vec <- G.replicateM n getElement
    ci <- G.replicateM n $ fromIntegral <$> getWord64le
    rp <- G.replicateM c $ fromIntegral <$> getWord64le
    return $ SM.CSR r c vec ci rp
{-# INLINE getCSR #-}
