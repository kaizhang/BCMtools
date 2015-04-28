{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
module BCM.IOMatrix
    ( IOMatrix(..)
    , DMatrix
    , DSMatrix
    , MMatrix
    , MSMatrix
    , MCSR
    ) where

import Control.Monad (when, forM_)
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy as L
import Data.Binary (Binary, encode, decode)

import qualified Data.Matrix.Generic as MG
import qualified Data.Matrix.Generic.Mutable as MGM
import Data.Matrix.Dense.Generic (Matrix(..))
import Data.Matrix.Sparse.Generic (CSR(..), Zero(..))
import Data.Matrix.Symmetric (SymMatrix(..))

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Conduit (($$), ($=), Sink)
import qualified Data.Conduit.List as CL
import Text.Printf (printf)
import System.IO

import qualified BCM.DiskMatrix as DM

type MMatrix = IOMat (Matrix U.Vector) Double
type MSMatrix = IOMat (SymMatrix U.Vector) Double
type MCSR = IOSMat (CSR U.Vector) Double

type DMatrix = IODMat DM.DMatrix Double
type DSMatrix = IODMat DM.DSMatrix Double

class IOMatrix mat (t :: * -> *) a where
    dim :: mat t a -> (Int, Int)

    unsafeIndexM :: MonadIO io => mat t a -> (Int, Int) -> io a
    
    unsafeTakeRowM :: (U.Unbox a, MonadIO io) => mat t a -> Int -> io (U.Vector a)
    
    -- | Read a matrix from file handle
    hReadMatrix :: MonadIO io
                => Handle
                -> io (mat t a)

    hSaveMatrix :: MonadIO io => Handle -> mat t a -> io ()
    hSaveMatrix _ _ = return ()

    hCreateMatrix :: MonadIO io
                  => Handle  -- ^ file handle
                  -> (Int, Int)  -- ^ matrix dimension
                  -> Maybe Int  -- ^ number of non-zero elements
                  -> Sink ((Int,Int), a) io (mat t a)

-- | Just a wrapper
newtype IODMat m a = IODMat { unwrapD :: m a }

instance (DM.DiskData a, DM.DiskMatrix m a) => IOMatrix IODMat m a where
    dim = DM.dim . unwrapD
    {-# INLINE dim #-}

    unsafeIndexM = DM.unsafeRead . unwrapD
    {-# INLINE unsafeIndexM #-}

    unsafeTakeRowM = DM.unsafeReadRow . unwrapD
    {-# INLINE unsafeTakeRowM #-}

    hReadMatrix handle = do 
        r <- DM.hReadMatrixEither handle
        case r of
            Left e -> error e
            Right m -> return $ IODMat m
    {-# INLINE hReadMatrix #-}

    hCreateMatrix handle (r,c) _ = do
        mat <- DM.replicate handle (r,c) DM.zero
        CL.mapM_ $ \((i,j), x) -> DM.unsafeWrite mat (i,j) x
        return $ IODMat mat
    {-# INLINE hCreateMatrix #-}

-- | Just a wrapper
newtype IOMat m a = IOMat { unwrap :: m a }

instance ( U.Unbox a
         , DM.DiskData a
         , Binary (m U.Vector a)
         , MG.Matrix m U.Vector a
         ) => IOMatrix IOMat (m U.Vector) a where
    dim = MG.dim . unwrap
    {-# INLINE dim #-}

    unsafeIndexM (IOMat mat) (i,j) = return $ MG.unsafeIndex mat (i,j)
    {-# INLINE unsafeIndexM #-}

    unsafeTakeRowM (IOMat mat) i = return $ MG.takeRow mat i
    {-# INLINE unsafeTakeRowM #-}

    hReadMatrix handle = liftIO $ do
        mat <- decode <$> L.hGetContents handle
        return $ IOMat mat
    {-# INLINE hReadMatrix #-}

    hSaveMatrix handle (IOMat mat) = liftIO . L.hPutStr handle . encode $ mat
    {-# INLINE hSaveMatrix #-}

    hCreateMatrix _ (r,c) _ = do
        mat <- liftIO $ MGM.replicate (r,c) DM.zero
        CL.mapM_ $ \((i,j), x) -> liftIO $ MGM.unsafeWrite mat (i,j) x
        mat' <- liftIO $ MG.unsafeFreeze mat
        return $ IOMat mat'
    {-# INLINE hCreateMatrix #-}

-- | Just a wrapper
newtype IOSMat m a = IOSMat { unwrapS :: m a }

instance ( Zero a
         , U.Unbox a
         , DM.DiskData a
         , Binary (CSR U.Vector a)
         ) => IOMatrix IOSMat (CSR U.Vector) a where
    dim = MG.dim . unwrapS
    {-# INLINE dim #-}

    unsafeIndexM (IOSMat mat) (i,j) = return $ (MG.!) mat (i,j)
    {-# INLINE unsafeIndexM #-}

    unsafeTakeRowM (IOSMat mat) i = return $ MG.takeRow mat i
    {-# INLINE unsafeTakeRowM #-}

    hReadMatrix handle = liftIO $ do
        mat <- decode <$> L.hGetContents handle
        return $ IOSMat mat
    {-# INLINE hReadMatrix #-}

    hSaveMatrix handle (IOSMat mat) = liftIO . L.hPutStr handle . encode $ mat
    {-# INLINE hSaveMatrix #-}

    hCreateMatrix _ (r,c) (Just n) = do
        v <- liftIO $ UM.new n
        col <- liftIO $ UM.new n
        row <- liftIO $ UM.new (r+1)

        ((i,_),_) <- flip CL.foldM ((-1,-1),0) $ \((i',j'), acc) ((i,j),x) ->
            if i > i' || (i == i' && j > j')
               then liftIO $ do
                   UM.write v acc x
                   UM.write col acc j
                   let stride = i - i'
                   when (stride > 0) $ forM_ [0..stride-1] $ \s -> UM.write row (i-s) acc
            
                   return ((i,j), acc+1)
               else error $ printf "Input must be sorted by row and then by column: (%d,%d) >= (%d,%d)" i' j' i j

        let stride = r - i
        liftIO $ forM_ [0..stride-1] $ \s -> UM.write row (r-s) n

        v' <- liftIO $ U.unsafeFreeze v
        col' <- liftIO $ U.unsafeFreeze col
        row' <- liftIO $ U.unsafeFreeze row

        return $ IOSMat $ CSR r c v' col' row'
    hCreateMatrix _ _ _ = error "no length info available"
    {-# INLINE hCreateMatrix #-}
