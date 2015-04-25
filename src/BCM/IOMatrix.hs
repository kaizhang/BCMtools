{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module BCM.IOMatrix
    ( IOMatrix(..)
    , DM.DMatrix
    , DM.DSMatrix
    , MMatrix
    , MSMatrix
    , MCSR
    ) where

import Control.Monad (when, forM_)
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Matrix.Generic as MG
import qualified Data.Matrix.Generic.Mutable as MGM

import Data.Matrix.Dense.Generic (Matrix(..))
import Data.Matrix.Sparse.Generic (CSR(..), Zero(..))
import Data.Matrix.Symmetric (SymMatrix(..))

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Lazy as L
import Data.Binary.Get
import Data.Conduit
import qualified Data.Conduit.List as CL
import System.IO
import Text.Printf (printf)

import qualified BCM.DiskMatrix as DM

type MMatrix = Matrix U.Vector
type MSMatrix = SymMatrix U.Vector
type MCSR = CSR U.Vector

class IOMatrix mat a where
    dim :: mat a -> (Int, Int)

    unsafeIndexM :: MonadIO io => mat a -> (Int, Int) -> io a
    
    unsafeTakeRowM :: (U.Unbox a, MonadIO io) => mat a -> Int -> io (U.Vector a)
    
    hReadMatrix :: MonadIO io => Handle -> Integer -> io (mat a)

    hSaveMatrix :: MonadIO io => Handle -> mat a -> io ()
    hSaveMatrix _ _ = return ()

    hCreateMatrix :: MonadIO io
                  => Handle  -- ^ file handle
                  -> Integer -- ^ handle offset
                  -> (Int, Int)  -- ^ matrix dimension
                  -> Maybe Int  -- ^ number of non-zero elements
                  -> Sink ((Int,Int), a) io (mat a)

instance DM.DiskData a => IOMatrix DM.DMatrix a where
    dim = DM.dim

    unsafeIndexM = DM.unsafeRead

    unsafeTakeRowM = DM.unsafeReadRow

    hReadMatrix handle p = do 
        r <- DM.hReadMatrixEither handle p
        case r of
            Left e -> error e
            Right m -> return m

    hCreateMatrix handle p (r,c) _ = do
        liftIO $ hSeek handle AbsoluteSeek p
        mat <- DM.replicate handle (r,c) DM.zero
        CL.mapM_ $ \((i,j), x) -> DM.unsafeWrite mat (i,j) x
        return mat

instance DM.DiskData a => IOMatrix DM.DSMatrix a where
    dim = DM.dim

    unsafeIndexM = DM.unsafeRead

    unsafeTakeRowM = DM.unsafeReadRow

    hReadMatrix handle p = do 
        r <- DM.hReadMatrixEither handle p
        case r of
            Left e -> error e
            Right m -> return m

    hCreateMatrix handle p (r,c) _ = do
        liftIO $ hSeek handle AbsoluteSeek p
        mat <- DM.replicate handle (r,c) DM.zero
        CL.mapM_ $ \((i,j), x) -> DM.unsafeWrite mat (i,j) x
        return mat

instance (U.Unbox a, DM.DiskData a) => IOMatrix (Matrix U.Vector) a where
    dim = MG.dim

    unsafeIndexM mat (i,j) = return $ MG.unsafeIndex mat (i,j)

    unsafeTakeRowM mat i = return $ MG.takeRow mat i

    hReadMatrix handle p = liftIO $ do
        hSeek handle AbsoluteSeek p
        magic <- runGet getWord32le <$> L.hGet handle 4
        if magic == DM.dmat_magic
           then do
               r <- DM.hRead1 handle
               c <- DM.hRead1 handle
               vec <- U.replicateM (r*c) $ DM.hRead1 handle
               return $ MG.unsafeFromVector (r,c) vec
           else error "Read fail: wrong signature"

    hCreateMatrix _ _ (r,c) _ = do
        mat <- liftIO $ MGM.replicate (r,c) DM.zero
        CL.mapM_ $ \((i,j), x) -> liftIO $ MGM.unsafeWrite mat (i,j) x
        liftIO $ MG.unsafeFreeze mat

instance (U.Unbox a, DM.DiskData a) => IOMatrix (SymMatrix U.Vector) a where
    dim = MG.dim

    unsafeIndexM mat (i,j) = return $ MG.unsafeIndex mat (i,j)

    unsafeTakeRowM mat i = return $ MG.takeRow mat i

{-
    hReadMatrix handle p = liftIO $ do
        hSeek handle AbsoluteSeek p
        magic <- runGet getWord32le <$> L.hGet handle 4
        if magic == DM.dsmat_magic
           then do
               n <- DM.hRead1 handle
               vec <- U.replicateM (r*c) $ DM.hRead1 handle
               return $ MG.unsafeFromVector (r,c) vec
           else error "Read fail: wrong signature"
           -}

    hCreateMatrix _ _ (r,c) _ = do
        mat <- liftIO $ MGM.replicate (r,c) DM.zero
        CL.mapM_ $ \((i,j), x) -> liftIO $ MGM.unsafeWrite mat (i,j) x
        liftIO $ MG.unsafeFreeze mat

instance (Zero a, U.Unbox a, DM.DiskData a) => IOMatrix (CSR U.Vector) a where
    dim = MG.dim

    unsafeIndexM mat (i,j) = return $ (MG.!) mat (i,j)

    unsafeTakeRowM mat i = return $ MG.takeRow mat i

    hReadMatrix handle p = liftIO $ do
        hSeek handle AbsoluteSeek p
        magic <- runGet getWord32le <$> L.hGet handle 4
        if magic == DM.dmat_magic
           then do
               r <- DM.hRead1 handle
               c <- DM.hRead1 handle
               vec <- U.replicateM (r*c) $ DM.hRead1 handle
               return $ MG.unsafeFromVector (r,c) vec
           else error "Read fail: wrong signature"

    hCreateMatrix _ _ (r,c) (Just n) = do
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

        return $ CSR r c v' col' row'
    hCreateMatrix _ _ _ _ = error "no length info available"
