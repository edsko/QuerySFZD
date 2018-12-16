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

rawSearch :: CdwQuery
          -> CDW SearchChar
          -> CDW Author
          -> CDW Style
          -> Maybe CdwReferer
          -> ClientM CdwResults

nextPage :: DynPath -> ClientM CdwResults

(rawSearch :<|> nextPage) = client api

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

search :: Manager
       -> Cache
       -> Query
       -> IO (Either ServantError Results)
search mgr cache Query{..} =
    runExceptT $ goChars queryChars
  where
    goChars :: SearchChars -> ExceptT ServantError IO Results
    goChars (SearchChars cs) = nubResults . mconcat <$> mapM goChar cs

    goChar :: SearchChar -> ExceptT ServantError IO Results
    goChar c = do
         mCached <- liftIO $ cacheLookup cache c
         case mCached of
           Just cs ->
             return Results {
                   resultsChars = Map.singleton c cs
                 , resultsRaw   = RawResult []
                 }
           Nothing -> do
             results <- goChar' c
             liftIO $ addToCache cache c (resultsChars results Map.! c)
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
    rawSearch' c = fromCdwResults Nothing c <$>
        rawSearch
          CdwCalligraphy
          (CDW c)
          (CDW (Author ""))
          (CDW queryStyle)
          (Just CdwRefererSelf)

    nextPage' :: DynPath -> SearchChar -> ClientM (Results, Maybe DynPath)
    nextPage' p c = fromCdwResults (Just p) c <$> nextPage p

    fromCdwResults :: Maybe DynPath
                   -> SearchChar
                   -> CdwResults
                   -> (Results, Maybe DynPath)
    fromCdwResults mp c CdwResults{..} = (
          Results {
              resultsChars = Map.singleton c cdwCharacters
            , resultsRaw   = RawResult [(header, cdwRaw)]
            }
        , cdwNextPage
        )
      where
        header :: String
        header = case mp of
                   Nothing -> "search '" ++ [searchChar c] ++ "'"
                   Just p  -> dynPathToString p

    clientEnvSearch, clientEnvNext :: ClientEnv
    clientEnvSearch = mkClientEnv mgr baseUrlSearch
    clientEnvNext   = mkClientEnv mgr baseUrlNext
