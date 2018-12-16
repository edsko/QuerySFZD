module QuerySFZD.Cache (
    Cache -- opaque
  , withCache
  , cacheChar
  , getCachedChar
  , cachePreference
  , getCachedPreferences
  ) where

import Codec.Serialise
import Control.Exception
import Data.Foldable (forM_)
import System.Directory
import System.FilePath

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Ours.Results
import QuerySFZD.Preferences (Preferences)

import qualified QuerySFZD.Preferences as Preferences

{-------------------------------------------------------------------------------
  Cache initialization
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Access
-------------------------------------------------------------------------------}

cacheChar :: Cache -> Style -> SearchChar -> [Character] -> IO ()
cacheChar cache style c cs = writeFileSerialise path cs
  where
    path = mkPath cache style c

getCachedChar :: Cache -> Style -> SearchChar -> IO (Maybe [Character])
getCachedChar cache style c = do
    exists <- doesFileExist path
    if exists
      then Just <$> readFileDeserialise path
      else return Nothing
  where
    path = mkPath cache style c

cachePreference :: Cache -> String -> IO ()
cachePreference cache url = do
    -- TODO: Potential race condition here
    ps <- getCachedPreferences cache
    writeFileSerialise path $ Preferences.prefer url ps
  where
    path = preferencesPath cache

getCachedPreferences :: Cache -> IO Preferences
getCachedPreferences cache = do
    exists <- doesFileExist path
    if exists
      then readFileDeserialise path
      else return Preferences.empty
  where
    path = preferencesPath cache

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

mkPath :: Cache -> Style -> SearchChar -> FilePath
mkPath (Cache cache) style (SearchChar c) =
    cache </> show style </> [c]

preferencesPath :: Cache -> FilePath
preferencesPath (Cache cache) = cache </> "preferences"
