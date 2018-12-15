{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module QuerySFZD.API.Ours (
    API
  , Characters(..)
  , api
  , module Export
  ) where

import Data.Proxy
import Servant
import Servant.HTML.Blaze

import QuerySFZD.API.Ours.IndexPage as Export
import QuerySFZD.API.Ours.Results as Export

-- | Characters we're seaching for
newtype Characters = Characters String
  deriving newtype FromHttpApiData

type API = Get '[HTML] IndexPage
      :<|> "results"
        :> QueryParam' '[Required] "characters" Characters
        :> Get '[HTML] Results

api :: Proxy API
api = Proxy
