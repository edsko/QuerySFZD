{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module QuerySFZD.API.Ours (
    API
  , api
  , module Export
  ) where

import           Data.Proxy
import           Servant
import           Servant.HTML.Blaze

import           QuerySFZD.API.Ours.Query   as Export
import           QuerySFZD.API.Ours.Results as Export

type API = Get '[HTML] Query
      :<|> "results" :> QueryParam' '[Required] "characters" String :> Get '[HTML] Results

api :: Proxy API
api = Proxy
