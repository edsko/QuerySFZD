{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}

module QuerySFZD.Client.CiDianWang (
    search
  ) where

import Control.Concurrent
import Control.Monad.Except
import Network.HTTP.Client (Manager)
import Servant
import Servant.Client hiding (baseUrl)

import qualified Data.Map.Strict as Map

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
       -> IO (Either ServantError Results)
search mgr cache style = runExceptT . goChars
  where
    goChars :: SearchChars -> ExceptT ServantError IO Results
    goChars (SearchChars cs) = nubResults . mconcat <$> mapM goChar cs

    goChar :: SearchChar -> ExceptT ServantError IO Results
    goChar c = do
         mCached <- liftIO $ cacheLookup cache c
         case mCached of
           Just cs ->
             return Results {
                   searchChars = SearchChars [c]
                 , resultChars = Map.singleton c cs
                 , rawResult   = RawResult []
                 }
           Nothing -> do
             results <- goChar' c
             liftIO $ addToCache cache c (resultChars results Map.! c)
             return results

    goChar' :: SearchChar -> ExceptT ServantError IO Results
    goChar' c = do
        (first, next) <- ExceptT $ runClientM (rawSearch'  c) clientEnvSearch
        rest          <- ExceptT $ runClientM (goNext next c) clientEnvNext
        return $ first <> rest

    goNext :: Maybe DynPath -> SearchChar -> ClientM Results
    goNext Nothing  _ = return mempty
    goNext (Just p) c = do
        liftIO $ threadDelay 2_000_000 -- don't flood the server
        (here, next) <- nextPage' p c
        rest         <- goNext next c
        return $ here <> rest

    rawSearch' :: SearchChar -> ClientM (Results, Maybe DynPath)
    rawSearch' c = do
        CdwResults{..} <- rawSearch
          (CDW Calligraphy)
          (CDW c)
          (CDW (Author ""))
          (CDW style)
          (Just (CDW RefererSelf))
        return (
            Results {
                searchChars = SearchChars [c]
              , resultChars = Map.singleton c cdwCharacters
              , rawResult   = RawResult [(header, cdwRaw)]
              }
          , cdwNextPage
          )
      where
        header = "search '" ++ [searchChar c] ++ "'"

    nextPage' :: DynPath -> SearchChar -> ClientM (Results, Maybe DynPath)
    nextPage' p c = do
        CdwResults{..} <- nextPage p
        return (
            Results {
                searchChars = SearchChars [] -- don't repeat characters
              , resultChars = Map.singleton c cdwCharacters
              , rawResult   = RawResult [(header, cdwRaw)]
              }
          , cdwNextPage
          )
      where
        header = dynPathToString p

    clientEnvSearch, clientEnvNext :: ClientEnv
    clientEnvSearch = mkClientEnv mgr baseUrlSearch
    clientEnvNext   = mkClientEnv mgr baseUrlNext
