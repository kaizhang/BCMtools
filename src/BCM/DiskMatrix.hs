{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BCM.DiskMatrix
    ( Offset
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
import qualified Data.ByteString as B
import Data.Bits (shiftR)
import Data.Int
import Data.Word
import qualified Data.Vector.Generic as G
import System.IO
import BCM.Binary

type Offset = Integer

-- | Matrix stored in binary file
class BinaryData a => DiskMatrix m a where
    elemType :: m a -> a
    elemType = undefined
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

data DMatrix a = DMatrix !Int  -- rows
                         !Int  -- cols
                         !Offset -- offset
                         !Bool   -- byte swap
                         !Handle  -- file handle

instance (Swappable a, BinaryData a) => DiskMatrix DMatrix a where
    hReadMatrixEither h = liftIO $ do
        p <- hTell h
        magic <- hGetData h False
        let byteSwapped | magic == d_matrix_magic = False
                        | byteSwap32 magic == d_matrix_magic = True
                        | otherwise = error "Read matrix fail: wrong signature"
        r <- fromIntegral <$> (hGetData h byteSwapped :: IO Int64)
        c <- fromIntegral <$> (hGetData h byteSwapped :: IO Int64)
        return $ Right $ DMatrix r c (p+20) byteSwapped h
    {-# INLINE hReadMatrixEither #-}

    dim (DMatrix r c _ _ _) = (r,c)
    {-# INLINE dim #-}

    replicate h (r,c) x = liftIO $ do
        p <- hTell h
        hPutData h d_matrix_magic
        hPutData h (fromIntegral r :: Word64)
        hPutData h (fromIntegral c :: Word64)
        replicateM_ (r*c) $ hPutData h x
        return $ DMatrix r c (p+20) False h
    {-# INLINE replicate #-}

    unsafeRead mat@(DMatrix _ c offset byteSwapped h) (i,j) = liftIO $ do
        hSeek h AbsoluteSeek $ offset + fromIntegral (size (elemType mat) * idx c i j)
        hGetData h byteSwapped
    {-# INLINE unsafeRead #-}

    unsafeWrite mat@(DMatrix _ c offset byteSwapped h) (i,j) x = liftIO $ do
        hSeek h AbsoluteSeek $ offset + fromIntegral (size (elemType mat) * idx c i j)
        let x' | byteSwapped = byteSwap x
               | otherwise = x
        hPutData h x'
    {-# INLINE unsafeWrite #-}

    unsafeReadRow mat@(DMatrix _ c offset byteSwapped h) i = liftIO $ do
        hSeek h AbsoluteSeek $ offset + fromIntegral (s * c * i)
        G.replicateM c $ hGetData h byteSwapped
      where
        s = size $ elemType mat
    {-# INLINE unsafeReadRow #-}

    close (DMatrix _ _ _ _ h) = liftIO $ hClose h

-- | Symmetric matrix
data DSMatrix a = DSMatrix !Int     -- size
                           !Offset  -- offset
                           !Bool    -- byteSwapped
                           !Handle  -- file handle

instance (Swappable a, BinaryData a) => DiskMatrix DSMatrix a where
    hReadMatrixEither h = liftIO $ do
        p <- hTell h
        magic <- hGetData h False
        let byteSwapped | magic == ds_matrix_magic = False
                        | byteSwap32 magic == ds_matrix_magic = True
                        | otherwise = error "Read matrix fail: wrong signature"
        n <- fromIntegral <$> (hGetData h byteSwapped :: IO Int64)
        return $ Right $ DSMatrix n (p+12) byteSwapped h
    {-# INLINE hReadMatrixEither #-}

    dim (DSMatrix n _ _ _) = (n,n)
    {-# INLINE dim #-}

    replicate h (r,c) x
        | r /= c = error "Not a sqaure matrix"
        | otherwise = liftIO $ do
            p <- hTell h
            hPutData h ds_matrix_magic
            hPutData h (fromIntegral r :: Int64)
            replicateM_ (((r+1)*r) `shiftR` 1) $ hPutData h x
            return $ DSMatrix r (p+12) False h
    {-# INLINE replicate #-}

    unsafeRead mat@(DSMatrix n offset byteSwapped h) (i,j) = liftIO $ do
        hSeek h AbsoluteSeek $ offset + fromIntegral (size (elemType mat) * idx' n i j)
        hGetData h byteSwapped
    {-# INLINE unsafeRead #-}

    unsafeWrite mat@(DSMatrix n offset byteSwapped h) (i,j) x = liftIO $ do
        hSeek h AbsoluteSeek $ offset + fromIntegral (size (elemType mat) * idx' n i j)
        let x' | byteSwapped = byteSwap x
               | otherwise = x
        hPutData h x'
    {-# INLINE unsafeWrite #-}

    close (DSMatrix _ _ _ h) = liftIO $ hClose h

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

