module QuerySFZD.Client.ShuFaZiDian (
    search
  ) where

import Network.HTTP.Client (Manager)
-- import Servant
import Servant.Client hiding (baseUrl)

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Ours.Results
import QuerySFZD.Cache

search :: Manager
       -> Cache
       -> Query
       -> IO (Either ServantError Results)
search = undefined
