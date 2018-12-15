{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module QuerySFZD.API.Theirs.CiDianWang (
    API
  , api
  , module Export
  ) where

import Data.Proxy
import Servant
import Servant.HTML.Blaze

import QuerySFZD.API.Theirs.CiDianWang.Results as Export
import QuerySFZD.API.Theirs.Common

type API = QueryParam' '[Required] "q" SingleChar
        :> Get '[HTML] Results

api :: Proxy API
api = Proxy
