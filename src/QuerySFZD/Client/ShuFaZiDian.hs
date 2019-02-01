{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

module QuerySFZD.Client.ShuFaZiDian (
    search
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Except
import Data.Char (isLetter)
import Network.HTTP.Client (Manager)
import Servant.Client hiding (baseUrl)
import System.Random (randomRIO)

import qualified Data.Map.Strict as Map

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Ours.Results
import QuerySFZD.API.Theirs.ShuFaZiDian
import QuerySFZD.Cache
import QuerySFZD.Util

{-------------------------------------------------------------------------------
  Raw client
-------------------------------------------------------------------------------}

baseUrl :: BaseUrl
baseUrl = BaseUrl {
      baseUrlScheme = Http
    , baseUrlHost   = "shufazidian.com"
    , baseUrlPort   = 80
    , baseUrlPath   = ""
    }

rawSearch :: SfzdArgs -> ClientM SfzdResults
rawSearch = client api

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

search :: Manager
       -> Cache
       -> Query
       -> IO (Either ServantError Results)
search mgr cache Query{..} = do
    runExceptT $ goChars queryChars
  where
    goChars :: SearchChars -> ExceptT ServantError IO Results
    goChars (SearchChars cs) = nubResults . mconcat <$> mapM goChar cs

    goChar :: SearchChar -> ExceptT ServantError IO Results
    goChar c | not (isLetter (searchChar c)) =
        return Results {
            resultsChars = Map.singleton c []
          , resultsRaw   = RawResult []
          }
    goChar c = do
        mCached <- liftIO $ getCachedChar cache queryStyle c
        case mCached of
          Just cs ->
            return Results {
                  resultsChars = Map.singleton c cs
                , resultsRaw   = RawResult []
                }
          Nothing -> do
            results <- goChar' c
            liftIO $ cacheChar cache queryStyle c (resultsChars results Map.! c)
            return results

    goChar' :: SearchChar -> ExceptT ServantError IO Results
    goChar' c = do
        liftIO $ randomRIO (1_000_000, 5_000_000) >>= threadDelay
        ExceptT $ runClientM (rawSearch' c) clientEnv

    rawSearch' :: SearchChar -> ClientM Results
    rawSearch' c = fromSfzdResults c <$>
        rawSearch SfzdArgs {
            sfzdChar  = c
          , sfzdStyle = queryStyle
          }

    fromSfzdResults :: SearchChar
                    -> SfzdResults
                    -> Results
    fromSfzdResults c SfzdResults{..} =
        Results {
            resultsChars = Map.singleton c sfzdCharacters
          , resultsRaw   = RawResult [(header, sfzdRaw)]
          }
      where
        header :: String
        header = "search '" ++ [searchChar c] ++ "'"

    clientEnv :: ClientEnv
    clientEnv = mkClientEnv mgr baseUrl
