{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module HiC.IOMatrix where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Matrix.Generic as MG
import qualified Data.Matrix.Generic.Mutable as MGM
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Lazy as L
import Data.Binary.IEEE754 (getFloat64le, putFloat64le)
import Data.Binary.Put
import Data.Binary.Get
import Data.Word
import Data.Conduit
import qualified Data.Conduit.List as CL
import System.IO

import qualified HiC.DiskMatrix as DM

class IOMatrix mat a where
    dim :: mat a -> (Int, Int)

    unsafeIndexM :: MonadIO io => mat a -> (Int, Int) -> io a
    
    unsafeTakeRowM :: (U.Unbox a, MonadIO io) => mat a -> Int -> io (U.Vector a)
    
    hReadMatrix :: MonadIO io => Handle -> Integer -> io (mat a)

    hSaveMatrix :: MonadIO io => Handle -> mat a -> io ()
    hSaveMatrix _ _ = return ()

    hCreateMatrix :: MonadIO io => Handle -> Integer -> (Int, Int) -> Sink ((Int,Int), a) io (mat a)

instance (DM.DiskData a, DM.DiskMatrix mat a) => IOMatrix mat a where
    dim = DM.dim

    unsafeIndexM = DM.unsafeRead

    unsafeTakeRowM = DM.unsafeReadRow

    hReadMatrix handle p = do 
        r <- DM.hReadMatrixEither handle p
        case r of
            Left e -> error e
            Right m -> return m

    hCreateMatrix handle p (r,c) = do
        liftIO $ hSeek handle AbsoluteSeek p
        mat <- DM.replicate handle (r,c) DM.zero
        CL.mapM_ $ \((i,j), x) -> DM.unsafeWrite mat (i,j) x
        return mat

instance (U.Unbox a, DM.DiskData a, MG.Matrix mat U.Vector a) => IOMatrix (mat U.Vector) a where
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

    hCreateMatrix _ _ (r,c) = do
        mat <- liftIO $ MGM.replicate (r,c) DM.zero
        CL.mapM_ $ \((i,j), x) -> liftIO $ MGM.unsafeWrite mat (i,j) x
        liftIO $ MG.unsafeFreeze mat
