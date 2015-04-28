{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BCM.DiskMatrix
    ( Offset
    , DiskData(..)
    , DiskMatrix(..)
    , read
    , write
    , DMatrix(..)
    , DSMatrix(..)
    ) where

import Prelude hiding (replicate)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as L
import Data.Bits (shiftR)
import Data.Binary.IEEE754 (getFloat64le, putFloat64le)
import Data.Binary.Put (putWord32le, putWord64le, runPut)
import Data.Binary.Get (getWord32le, getWord64le, runGet)
import Data.Conduit (($$), ($=))
import qualified Data.Conduit.List as CL
import qualified Data.Vector.Generic as G
import System.IO
import Numeric (showHex)

import BCM.Matrix.Instances (d_matrix_magic, ds_matrix_magic)

type Offset = Integer

class DiskData a where
    -- | The default value
    zero :: a

    sizeOf :: a -> Int

    fromByteString :: L.ByteString -> a
    
    toByteString :: a -> L.ByteString

    hRead1 :: MonadIO m => Handle -> m a
    hRead1 h = liftIO $ fmap fromByteString $ L.hGet h $ sizeOf (undefined :: a)
    {-# INLINE hRead1 #-}

    hWrite1 :: MonadIO m => Handle -> a -> m ()
    hWrite1 h = liftIO . L.hPut h . toByteString
    {-# INLINE hWrite1 #-}

    {-# MINIMAL zero, sizeOf, fromByteString, toByteString #-}

instance DiskData Double where
    zero = 0.0

    sizeOf _ = 8
    {-# INLINE sizeOf #-}

    fromByteString = runGet getFloat64le
    {-# INLINE fromByteString #-}

    toByteString = runPut . putFloat64le
    {-# INLINE toByteString #-}

instance DiskData Int where
    zero = 0

    sizeOf _ = 8
    {-# INLINE sizeOf #-}

    fromByteString = fromIntegral . runGet getWord64le
    {-# INLINE fromByteString #-}

    toByteString = runPut . putWord64le . fromIntegral
    {-# INLINE toByteString #-}

-- | Matrix stored in binary file
class DiskData a => DiskMatrix m a where
    elemType :: m a -> a
    elemType _ = undefined
    {-# INLINE elemType #-}

    hReadMatrixEither :: MonadIO io => Handle -> io (Either String (m a))

    dim :: m a -> (Int, Int)

    replicate :: MonadIO io => Handle -> (Int, Int) -> a -> io (m a)

    unsafeRead :: MonadIO io => m a -> (Int, Int) -> io a

    unsafeWrite :: MonadIO io => m a -> (Int, Int) -> a -> io ()

    unsafeReadRow :: (G.Vector v a, MonadIO io) => m a -> Int -> io (v a)
    unsafeReadRow mat i = G.generateM c $ \j -> unsafeRead mat (i,j)
      where
        (_,c) = dim mat
    {-# INLINE unsafeReadRow #-}

    close :: MonadIO io => m a -> io ()

-- Derived methods

write :: (MonadIO io, DiskMatrix m a) => m a -> (Int, Int) -> a -> io ()
write mat (i,j) x | i >= r || j >= c = error "Index out of bounds"
                  | otherwise = unsafeWrite mat (i,j) x
  where
    (r,c) = dim mat
{-# INLINE write #-}

data DMatrix a = DMatrix !Int  -- ^ rows
                         !Int  -- ^ cols
                         !Offset -- offset
                         !Handle  -- ^ file handle

instance DiskData a => DiskMatrix DMatrix a where
    hReadMatrixEither h = liftIO $ do
        p <- hTell h
        magic <- runGet getWord32le <$> L.hGet h 4
        if magic == d_matrix_magic
           then do
               r <- hRead1 h
               c <- hRead1 h
               return $ Right $ DMatrix r c (p+20) h
           else return $ Left $ "Read fail: wrong signature: 0x" ++ showHex magic ""
    {-# INLINE hReadMatrixEither #-}

    dim (DMatrix r c _ _) = (r,c)
    {-# INLINE dim #-}

    replicate h (r,c) x = liftIO $ do
        p <- hTell h
        L.hPut h $ runPut $ putWord32le d_matrix_magic
        hWrite1 h r
        hWrite1 h c
        replicateM_ (r*c) $ hWrite1 h x
        return $ DMatrix r c (p+20) h
    {-# INLINE replicate #-}

    unsafeRead mat@(DMatrix _ c offset h) (i,j) = liftIO $ do
        hSeek h AbsoluteSeek $ offset + fromIntegral (sizeOf (elemType mat) * idx c i j)
        hRead1 h
    {-# INLINE unsafeRead #-}

    unsafeWrite mat@(DMatrix _ c offset h) (i,j) x = liftIO $ do
        hSeek h AbsoluteSeek $ offset + fromIntegral (sizeOf (elemType mat) * idx c i j)
        hWrite1 h x
    {-# INLINE unsafeWrite #-}

    unsafeReadRow mat@(DMatrix _ c offset h) i = liftIO $ do
        hSeek h AbsoluteSeek $ offset + fromIntegral (size * c * i)
        G.replicateM c $ hRead1 h
      where
        size = sizeOf $ elemType mat
    {-# INLINE unsafeReadRow #-}

    close (DMatrix _ _ _ h) = liftIO $ hClose h

-- | Symmetric matrix
data DSMatrix a = DSMatrix !Int  -- ^ size
                           !Offset  -- ^ offset
                           !Handle  -- ^ file handle

instance DiskData a => DiskMatrix DSMatrix a where
    hReadMatrixEither h = liftIO $ do
        p <- hTell h
        magic <- runGet getWord32le <$> L.hGet h 4
        if magic == ds_matrix_magic
           then do
               n <- hRead1 h
               return $ Right $ DSMatrix n (p+12) h
           else return $ Left $ "Read fail: wrong signature: 0x" ++ showHex magic ""
    {-# INLINE hReadMatrixEither #-}

    dim (DSMatrix n _ _) = (n,n)
    {-# INLINE dim #-}

    replicate h (r,c) x
        | r /= c = error "Not a sqaure matrix"
        | otherwise = liftIO $ do
            p <- hTell h
            L.hPut h $ runPut $ putWord32le ds_matrix_magic
            hWrite1 h r
            replicateM_ (((r+1)*r) `shiftR` 1) $ hWrite1 h x
            return $ DSMatrix r (p+12) h
    {-# INLINE replicate #-}

    unsafeRead mat@(DSMatrix n offset h) (i,j) = liftIO $ do
        hSeek h AbsoluteSeek $ offset + fromIntegral (sizeOf (elemType mat) * idx' n i j)
        hRead1 h
    {-# INLINE unsafeRead #-}

    unsafeWrite mat@(DSMatrix n offset h) (i,j) x = liftIO $ do
        hSeek h AbsoluteSeek $ offset + fromIntegral (sizeOf (elemType mat) * idx' n i j)
        hWrite1 h x
    {-# INLINE unsafeWrite #-}

    close (DSMatrix _ _ h) = liftIO $ hClose h

------------------------------------------------------------------------------
-- helper functions
------------------------------------------------------------------------------

-- normal matrix indexing
idx :: Int -> Int -> Int -> Int
idx c i j = i * c + j
{-# INLINE idx #-}

-- upper triangular matrix indexing
idx' :: Int -> Int -> Int -> Int
idx' n i j | i <= j = (i * (2 * n - i - 1)) `shiftR` 1 + j
           | otherwise = (j * (2 * n - j - 1)) `shiftR` 1 + i
{-# INLINE idx' #-}
