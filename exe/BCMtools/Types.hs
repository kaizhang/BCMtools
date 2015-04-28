module BCMtools.Types
    ( BCMtoolsOptions(..)
    , Command(..)
    , ConvertOptions(..)
    , ViewOptions(..)
    ) where

data BCMtoolsOptions = BCMtoolsOptions
    { _input :: FilePath
    , _output :: FilePath
    , _command :: Command
    }

data Command = Convert ConvertOptions
             | View ViewOptions

data ConvertOptions = ConvertOptions
    { _genome :: String    -- ^ genome
    , _rownames :: [String]  -- ^ row chrs
    , _colnames :: [String]  -- ^ column chrs
    , _resolution :: Int   -- ^ resolution
    , _sparse :: Bool
    , _symmetric :: Bool
    } deriving (Show)

data ViewOptions = ViewOptions
    { _valueRange :: (Double, Double)
    , _inMem :: Bool
    }
