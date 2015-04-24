{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HiC.DiskMatrix
    ( Offset
    , DiskData(..)
    , DiskMatrix(..)
    , read
    , write
    , DMatrix(..)
    , DSMatrix(..)
    , fromStream
    , fromList
    ) where

import Prelude hiding (replicate)
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as L
import Data.Bits (shiftR)
import Data.Binary.IEEE754 (getFloat64le, putFloat64le)
import Data.Binary.Put
import Data.Binary.Get
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Word
import System.IO

type Offset = Integer

class DiskData a where
    -- | The default value
    zero :: a

    size :: a -> Int

    fromByteString :: L.ByteString -> a
    
    toByteString :: a -> L.ByteString

    hRead1 :: Handle -> IO a
    hRead1 h = fmap fromByteString $ L.hGet h $ size (undefined :: a)
    {-# INLINE hRead1 #-}

    hWrite1 :: Handle -> a -> IO ()
    hWrite1 h = L.hPut h . toByteString
    {-# INLINE hWrite1 #-}

    {-# MINIMAL zero, size, fromByteString, toByteString #-}

instance DiskData Double where
    zero = 0.0

    size _ = 8
    {-# INLINE size #-}

    fromByteString = runGet getFloat64le
    {-# INLINE fromByteString #-}

    toByteString = runPut . putFloat64le
    {-# INLINE toByteString #-}

instance DiskData Int where
    zero = 0

    size _ = 8
    {-# INLINE size #-}

    fromByteString = fromIntegral . runGet getWord64le
    {-# INLINE fromByteString #-}

    toByteString = runPut . putWord64le . fromIntegral
    {-# INLINE toByteString #-}

-- | Matrix stored in binary file
class DiskMatrix m a where
    hReadMatrixEither :: Handle -> Offset -> IO (Either String (m a))

    dim :: m a -> (Int, Int)

    replicate :: Handle -> (Int, Int) -> a -> IO (m a)

    unsafeRead :: m a -> (Int, Int) -> IO a

    unsafeWrite :: m a -> (Int, Int) -> a -> IO ()

-- Derived methods

write :: DiskMatrix m a => m a -> (Int, Int) -> a -> IO ()
write mat (i,j) x | i >= r || j >= c = error "Index out of bounds"
                  | otherwise = unsafeWrite mat (i,j) x
  where
    (r,c) = dim mat
{-# INLINE write #-}

data DMatrix a = DMatrix !Int  -- ^ rows
                         !Int  -- ^ cols
                         !Offset -- offset
                         !Handle  -- ^ file handle

dmat_magic :: Word32
dmat_magic = 0x22D20B77
{-# INLINE dmat_magic #-}

instance DiskData a => DiskMatrix DMatrix a where
    hReadMatrixEither h p = do
        hSeek h AbsoluteSeek p
        magic <- runGet getWord32le <$> L.hGet h 4
        if magic == dmat_magic
           then do
               r <- hRead1 h
               c <- hRead1 h
               return $ Right $ DMatrix r c (p+20) h
           else return $ Left "Read fail: wrong signature"
    {-# INLINE hReadMatrixEither #-}

    dim (DMatrix r c _ _) = (r,c)
    {-# INLINE dim #-}

    replicate h (r,c) x = do
        p <- hTell h
        L.hPut h $ runPut $ putWord32le dmat_magic
        hWrite1 h r
        hWrite1 h c
        replicateM_ (r*c) $ hWrite1 h x
        return $ DMatrix r c (p+20) h
    {-# INLINE replicate #-}

    unsafeRead (DMatrix _ c offset h) (i,j) = do
        hSeek h AbsoluteSeek $ offset + idx c i j
        hRead1 h
    {-# INLINE unsafeRead #-}

    unsafeWrite (DMatrix _ c offset h) (i,j) x = do
        hSeek h AbsoluteSeek $ offset + idx c i j
        hWrite1 h x
    {-# INLINE unsafeWrite #-}

-- | Symmetric matrix
data DSMatrix a = DSMatrix !Int  -- ^ size
                           !Offset  -- ^ offset
                           !Handle  -- ^ file handle

dsmat_magic :: Word32
dsmat_magic = 0x33D31A66
{-# INLINE dsmat_magic #-}

instance DiskData a => DiskMatrix DSMatrix a where
    hReadMatrixEither h p = do
        hSeek h AbsoluteSeek p
        magic <- runGet getWord32le <$> L.hGet h 4
        if magic == dsmat_magic
           then do
               n <- hRead1 h
               return $ Right $ DSMatrix n (p+12) h
           else return $ Left "Read fail: wrong signature"
    {-# INLINE hReadMatrixEither #-}

    dim (DSMatrix n _ _) = (n,n)
    {-# INLINE dim #-}

    replicate h (r,c) x
        | r /= c = error "Not a sqaure matrix"
        | otherwise = do
            p <- hTell h
            L.hPut h $ runPut $ putWord32le dsmat_magic
            hWrite1 h r
            replicateM_ (((r+1)*r) `shiftR` 1) $ hWrite1 h x
            return $ DSMatrix r (p+12) h
    {-# INLINE replicate #-}

    unsafeRead (DSMatrix n offset h) (i,j) = do
        hSeek h AbsoluteSeek $ offset + idx' n i j
        hRead1 h
    {-# INLINE unsafeRead #-}

    unsafeWrite (DSMatrix n offset h) (i,j) x = do
        hSeek h AbsoluteSeek $ offset + idx' n i j
        hWrite1 h x
    {-# INLINE unsafeWrite #-}


-- Construction

fromStream :: DiskMatrix mat a
           => Handle
           -> a  -- ^ default value
           -> (Int, Int)  -- ^ matrix dimension
           -> Sink ((Int,Int), a) IO (mat a)
fromStream h x (r,c) = do
    m <- liftIO $ replicate h (r,c) x
    CL.mapM_ (\((i,j),v) -> write m (i,j) v)
    return m
{-# INLINE fromStream #-}

fromList :: DiskMatrix mat a
         => Handle
         -> (Int, Int)
         -> [a]
         -> IO (mat a)
fromList h (r,c) xs = undefined
{-# INLINE fromList #-}


------------------------------------------------------------------------------
-- helper functions
------------------------------------------------------------------------------

-- normal matrix indexing
idx :: Int -> Int -> Int -> Integer
idx c i j = fromIntegral $ 8 * (i * c + j)
{-# INLINE idx #-}

-- upper triangular matrix indexing
idx' :: Int -> Int -> Int -> Integer
idx' n i j | i <= j = fromIntegral $ (i * (2 * n - i - 1)) `shiftR` 1 + j
           | otherwise = fromIntegral $ (j * (2 * n - j - 1)) `shiftR` 1 + i
{-# INLINE idx' #-}
