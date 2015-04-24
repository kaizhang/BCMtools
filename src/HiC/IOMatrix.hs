{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module HiC.IOMatrix where

import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Matrix.Generic as MG
import qualified Data.Vector.Unboxed as U

import qualified HiC.DiskMatrix as DM

class MonadMatrix mat a where
    unsafeIndexM :: MonadIO m => mat a -> (Int, Int) -> m a
    
    readMatrix :: MonadIO m => FilePath -> m (mat a)

    saveMatrix :: MonadIO m => FilePath -> mat a -> m ()

instance DM.DiskMatrix mat a => MonadMatrix mat a where
    unsafeIndexM = DM.unsafeRead

instance MG.Matrix mat U.Vector a => MonadMatrix (mat U.Vector) a where
    unsafeIndexM mat (i,j) = return $ MG.unsafeIndex mat (i,j)
