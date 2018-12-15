module QuerySFZD.Client.CiDianWang (
    baseUrl
  , query
  ) where

import Servant.Client hiding (baseUrl)

import QuerySFZD.API.Theirs.CiDianWang
import QuerySFZD.API.Theirs.Common

baseUrl :: BaseUrl
baseUrl = BaseUrl {
      baseUrlScheme = Http
    , baseUrlHost   = "search.cidianwang.com"
    , baseUrlPort   = 80
    , baseUrlPath   = ""
    }

query :: SingleChar -> ClientM Results
query = client api
