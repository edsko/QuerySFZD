module QuerySFZD.Cache (
    Cache -- opaque
  , withCache
  , addToCache
  , cacheLookup
  ) where

import Codec.Serialise
import Control.Exception
import Data.Foldable (forM_)
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
    forM_ ([minBound .. maxBound] :: [Style]) $ \st ->
      createDirectoryIfMissing False ("cache" </> show st)
    return $ Cache "cache"

closeCache :: Cache -> IO ()
closeCache _ = return ()

addToCache :: Cache -> Style -> SearchChar -> [Character] -> IO ()
addToCache cache style c cs = writeFileSerialise path cs
  where
    path = mkPath cache style c

cacheLookup :: Cache -> Style -> SearchChar -> IO (Maybe [Character])
cacheLookup cache style c = do
    exists <- doesFileExist path
    if exists
      then Just <$> readFileDeserialise path
      else return Nothing
  where
    path = mkPath cache style c

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

mkPath :: Cache -> Style -> SearchChar -> FilePath
mkPath (Cache cache) style (SearchChar c) =
    cache </> show style </> [c]
