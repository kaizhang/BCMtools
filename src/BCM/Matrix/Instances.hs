{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module BCM.Matrix.Instances where

import Data.Bits (shiftR)
import Control.Applicative ((<$>))
import Data.Binary (Binary(..), Get, Put, Word32)
import Data.Binary.IEEE754 (getFloat64le, putFloat64le)
import Data.Binary.Get (getWord64le, getWord32le)
import Data.Binary.Put (putWord64le, putWord32le)
import Control.Monad (guard)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G

import qualified Data.Matrix.Dense.Generic as DM
import qualified Data.Matrix.Symmetric as DS
import qualified Data.Matrix.Sparse.Generic as SM

d_matrix_magic :: Word32
d_matrix_magic = 0x22D20B77

instance Binary (DM.Matrix U.Vector Double) where
    put = putMatrix putFloat64le
    get = getMatrix getFloat64le

instance Binary (DM.Matrix U.Vector Int) where
    put = putMatrix $ putWord64le . fromIntegral
    get = getMatrix $ fromIntegral <$> getWord64le

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

instance Binary (DS.SymMatrix U.Vector Double) where
    put = putSymMatrix putFloat64le
    get = getSymMatrix getFloat64le

instance Binary (DS.SymMatrix U.Vector Int) where
    put = putSymMatrix $ putWord64le . fromIntegral
    get = getSymMatrix $ fromIntegral <$> getWord64le

putSymMatrix :: G.Vector v a => (a -> Put) -> DS.SymMatrix v a -> Put
putSymMatrix putElement (DS.SymMatrix n vec) = do
    putWord32le ds_matrix_magic
    putWord64le . fromIntegral $ n
    G.mapM_ putElement vec
{-# INLINE putSymMatrix #-}

getSymMatrix :: G.Vector v a => Get a -> Get (DS.SymMatrix v a)
getSymMatrix getElement = do
    m <- getWord32le
    guard $ m == ds_matrix_magic
    n <- fromIntegral <$> getWord64le
    vec <- G.replicateM (((n+1)*n) `shiftR` 1) getElement
    return $ DS.SymMatrix n vec
{-# INLINE getSymMatrix #-}


sp_matrix_magic :: Word32
sp_matrix_magic = 0x177BFFA0

instance Binary (SM.CSR U.Vector Double) where
    put = putCSR putFloat64le
    get = getCSR getFloat64le

instance Binary (SM.CSR U.Vector Int) where
    put = putCSR $ putWord64le . fromIntegral
    get = getCSR $ fromIntegral <$> getWord64le

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
