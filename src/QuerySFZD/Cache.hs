module QuerySFZD.Cache (
    Cache -- opaque
  , withCache
  ) where

import Control.Exception
import System.Directory

data Cache = Cache

withCache :: (Cache -> IO a) -> IO a
withCache = bracket openCache closeCache

openCache :: IO Cache
openCache = do
    createDirectoryIfMissing False "cache"
    return Cache

closeCache :: Cache -> IO ()
closeCache Cache = return ()
