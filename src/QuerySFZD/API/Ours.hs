{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module QuerySFZD.API.Ours (
    API
  , api
  , module Export
  ) where

import Data.Proxy
import Servant
import Servant.HTML.Blaze

import QuerySFZD.API.Ours.IndexPage as Export
import QuerySFZD.API.Ours.Query as Export
import QuerySFZD.API.Ours.Results as Export
import QuerySFZD.Cache.Preferences (Prefer)
import QuerySFZD.Data.Calligraphers

type API = Get '[HTML] IndexPage
      :<|> "search" :> Search
      :<|> "prefer" :> ReqBody '[JSON] Prefer :> Post '[HTML] NoContent
      :<|> "static" :> Raw

type Search = QueryParam' '[Required] "backend"       Backend
           :> QueryParam' '[Required] "characters"    SearchChars
           :> QueryParam' '[Required] "style"         Style
           :> QueryParam' '[Required] "author"        CalligrapherName
           :> QueryParam' '[Required] "fallbacks"     Fallbacks
           :> QueryParam' '[Optional] "skipNotFound"  SkipNotFound
           :> QueryParam' '[Optional] "saveQuery"     SaveQuery
           :> QueryParam' '[Optional] "preferredOnly" PreferredOnly
           :> Get '[HTML] ResultsPage

api :: Proxy API
api = Proxy
