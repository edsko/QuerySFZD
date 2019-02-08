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
import QuerySFZD.API.Theirs.ShuFaZiDian
import QuerySFZD.Cache
import QuerySFZD.Client.Common

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
    runExceptT $ goStyles queryStyles
  where
    goStyles :: [Style] -> ExceptT ServantError IO Results
    goStyles styles = mconcat <$>
        mapM (\style -> goChars style querySearchChars) styles

    goChars :: Style -> SearchChars -> ExceptT ServantError IO Results
    goChars style (SearchChars cs) = nubResults . mconcat <$>
        mapM (goChar style) cs

    goChar :: Style -> SearchChar -> ExceptT ServantError IO Results
    goChar _style c | not (isLetter (searchChar c)) =
        return Results {
            resultsChars = Map.singleton c []
          , resultsRaw   = RawResult []
          }
    goChar style c = do
        mCached <- liftIO $ getCachedChar cache style c
        case mCached of
          Just cs ->
            return Results {
                  resultsChars = Map.singleton c cs
                , resultsRaw   = RawResult []
                }
          Nothing -> do
            results <- goChar' style c
            liftIO $ cacheChar cache style c (resultsChars results Map.! c)
            return results

    goChar' :: Style -> SearchChar -> ExceptT ServantError IO Results
    goChar' style c = do
        liftIO $ randomRIO (1_000_000, 5_000_000) >>= threadDelay
        ExceptT $ runClientM (rawSearch' style c) clientEnv

    rawSearch' :: Style -> SearchChar -> ClientM Results
    rawSearch' style c = fromSfzdResults c <$>
        rawSearch SfzdArgs {
            sfzdChar  = c
          , sfzdStyle = style
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
