{-# LANGUAGE RecordWildCards #-}

module QuerySFZD.Client.CiDianWang (
    search
  ) where

import Network.HTTP.Client (Manager)
import Servant.Client hiding (baseUrl)

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Ours.Results
import QuerySFZD.API.Theirs.CiDianWang

{-------------------------------------------------------------------------------
  Raw client
-------------------------------------------------------------------------------}

baseUrl :: BaseUrl
baseUrl = BaseUrl {
      baseUrlScheme = Http
    , baseUrlHost   = "search.cidianwang.com"
    , baseUrlPort   = 80
    , baseUrlPath   = ""
    }

rawSearch :: CDW Query
          -> CDW SearchChar
          -> CDW Author
          -> CDW Style
          -> Maybe (CDW Referer)
          -> ClientM CdwResults
rawSearch = client api

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

search :: Manager
       -> SearchChars
       -> Style
       -> IO (Either ServantError ([Character], RawResult))
search mgr (SearchChars [c]) s =
    runClientM go clientEnv
  where
    go :: ClientM ([Character], RawResult)
    go = do
        CdwResults{..} <- rawSearch
                            (CDW Calligraphy)
                            (CDW c)
                            (CDW (Author ""))
                            (CDW s)
                            (Just (CDW RefererSelf))
        return (characters, RawResult raw)

    clientEnv :: ClientEnv
    clientEnv = mkClientEnv mgr baseUrl
