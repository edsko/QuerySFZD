{-# LANGUAGE RecordWildCards #-}

module QuerySFZD.Client.CiDianWang (
    search
  ) where

import Control.Monad.Except
import Network.HTTP.Client (Manager)
import Servant
import Servant.Client hiding (baseUrl)

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Ours.Results
import QuerySFZD.API.Theirs.CiDianWang
import QuerySFZD.Cache
import QuerySFZD.Util

{-------------------------------------------------------------------------------
  Raw client
-------------------------------------------------------------------------------}

-- | Used for the initial search query
baseUrlSearch :: BaseUrl
baseUrlSearch = BaseUrl {
      baseUrlScheme = Http
    , baseUrlHost   = "search.cidianwang.com"
    , baseUrlPort   = 80
    , baseUrlPath   = ""
    }

-- | Used for all subsequent results pages
baseUrlNext :: BaseUrl
baseUrlNext = BaseUrl {
      baseUrlScheme = Http
    , baseUrlHost   = "www.cidianwang.com"
    , baseUrlPort   = 80
    , baseUrlPath   = "shufa"
    }

rawSearch :: CDW Query
          -> CDW SearchChar
          -> CDW Author
          -> CDW Style
          -> Maybe (CDW Referer)
          -> ClientM CdwResults

nextPage :: DynPath -> ClientM CdwResults

(rawSearch :<|> nextPage) = client api

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

search :: Manager
       -> Cache
       -> Style
       -> SearchChars
       -> IO (Either ServantError ([Character], RawResult))
search mgr cache style = runExceptT . goChars
  where
    goChars :: SearchChars -> ExceptT ServantError IO ([Character], RawResult)
    goChars (SearchChars cs) = mconcat <$> mapM goChar cs

    goChar :: SearchChar -> ExceptT ServantError IO ([Character], RawResult)
    goChar c = do
         mCached <- liftIO $ cacheLookup cache c
         case mCached of
           Just cs ->
             return (cs, mempty)
           Nothing -> do
             (cs, raw) <- goChar' c
             liftIO $ addToCache cache c cs
             return (cs, raw)

    goChar' :: SearchChar -> ExceptT ServantError IO ([Character], RawResult)
    goChar' c = do
        CdwResults{..} <- ExceptT $ runClientM (goSearch c) clientEnvSearch
        otherPages     <- ExceptT $ runClientM (goNext cdwNextPage) clientEnvNext
        let raw = RawResult [("search '" ++ [searchChar c] ++ "'", cdwRaw)]
        return $ (cdwCharacters, raw) <> otherPages

    goSearch :: SearchChar -> ClientM CdwResults
    goSearch c =
        rawSearch
          (CDW Calligraphy)
          (CDW c)
          (CDW (Author ""))
          (CDW style)
          (Just (CDW RefererSelf))

    goNext :: Maybe DynPath -> ClientM ([Character], RawResult)
    goNext Nothing  = return mempty
    goNext (Just p) = do
        CdwResults{..} <- nextPage p
        otherPages     <- goNext cdwNextPage
        let raw = RawResult [(dynPathToString p, cdwRaw)]
        return $ (cdwCharacters, raw) <> otherPages

    clientEnvSearch, clientEnvNext :: ClientEnv
    clientEnvSearch = mkClientEnv mgr baseUrlSearch
    clientEnvNext   = mkClientEnv mgr baseUrlNext
