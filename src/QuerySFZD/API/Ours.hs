{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

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

type API = Get '[HTML] IndexPage
      :<|> "search" :> Search
      :<|> "prefer" :> ReqBody '[PlainText] String :> Post '[HTML] NoContent
      :<|> "static" :> Raw

type Search = QueryParam' '[Required] "characters"   SearchChars
           :> QueryParam' '[Required] "style"        Style
           :> QueryParam' '[Required] "author"       Author
           :> QueryParam' '[Required] "fallbacks"    Fallbacks
           :> QueryParam' '[Optional] "skipNotFound" SkipNotFound
           :> QueryParam' '[Optional] "saveQuery"    SaveQuery
           :> Get '[HTML] ResultsPage

api :: Proxy API
api = Proxy
