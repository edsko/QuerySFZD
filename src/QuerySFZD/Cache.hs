module QuerySFZD.Cache (
    Cache -- opaque
  , withCache
  , addToCache
  , cacheLookup
  ) where

import Codec.Serialise
import Control.Exception
import System.Directory
import System.FilePath

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Ours.Results

-- | Opaque handle to the cache
--
-- Implementation note: right now the 'Cache' just carries the name of the
-- directory of the cache, since we simply store the characters as files in the
-- @cache@ directory. However, if for example we would move to a DB or
-- acid-state, we'd need to record a DB handle here.
data Cache = Cache FilePath

withCache :: (Cache -> IO a) -> IO a
withCache = bracket openCache closeCache

openCache :: IO Cache
openCache = do
    createDirectoryIfMissing False "cache"
    return $ Cache "cache"

closeCache :: Cache -> IO ()
closeCache _ = return ()

addToCache :: Cache -> SearchChar -> [Character] -> IO ()
addToCache (Cache cache) (SearchChar c) cs =
    writeFileSerialise (cache </> [c]) cs

cacheLookup :: Cache -> SearchChar -> IO (Maybe [Character])
cacheLookup (Cache cache) (SearchChar c) = do
    exists <- doesFileExist (cache </> [c])
    if exists
      then Just <$> readFileDeserialise (cache </> [c])
      else return Nothing
