{-# LANGUAGE RecordWildCards #-}

module QuerySFZD.Client.CiDianWang (
    search
  ) where

import Network.HTTP.Client (Manager)
import Servant
import Servant.Client hiding (baseUrl)
import Text.HTML.TagSoup (Tag)

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Ours.Results
import QuerySFZD.API.Theirs.CiDianWang
import QuerySFZD.Cache
import QuerySFZD.Util

{-------------------------------------------------------------------------------
  Raw client
-------------------------------------------------------------------------------}

baseUrlSearch :: BaseUrl
baseUrlSearch = BaseUrl {
      baseUrlScheme = Http
    , baseUrlHost   = "search.cidianwang.com"
    , baseUrlPort   = 80
    , baseUrlPath   = ""
    }

baseUrlResultsPage :: BaseUrl
baseUrlResultsPage = BaseUrl {
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

resultsPage :: DynPath -> ClientM CdwResults

(rawSearch :<|> resultsPage) = client api

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

search :: Manager
       -> Cache
       -> SearchChars
       -> Style
       -> IO (Either ServantError ([Character], RawResult))
search mgr cache (SearchChars [c]) s = do
    searchResult <- runClientM goSearch clientEnvSearch
    case searchResult of
      Left err ->
        return (Left err)
      Right CdwResults{..} -> do
        let accChars = characters
            accRaw   = [("search", raw)]
        runClientM (goNext accChars accRaw nextPage) clientEnvResultsPage
  where
    goSearch :: ClientM CdwResults
    goSearch = rawSearch
                 (CDW Calligraphy)
                 (CDW c)
                 (CDW (Author ""))
                 (CDW s)
                 (Just (CDW RefererSelf))

    goNext :: [Character]
           -> [(String, [Tag String])]
           -> Maybe DynPath
           -> ClientM ([Character], RawResult)
    goNext accChars accRaw Nothing =
        return (accChars, RawResult accRaw)
    goNext accChars accRaw (Just p) = do
        CdwResults{..} <- resultsPage p
        let accChars' = characters ++ accChars
            accRaw'   = (dynPathToString p, raw) : accRaw
        goNext accChars' accRaw' nextPage

    clientEnvSearch, clientEnvResultsPage :: ClientEnv
    clientEnvSearch      = mkClientEnv mgr baseUrlSearch
    clientEnvResultsPage = mkClientEnv mgr baseUrlResultsPage
