{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module QuerySFZD.API.Ours (
    API
  , api
  , renderQuery
  , module Export
  ) where

import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (Text)
import Servant
import Servant.HTML.Blaze

import QuerySFZD.API.Ours.Query as Export
import QuerySFZD.Cache.Preferences (Prefer)
import QuerySFZD.Data.Calligraphers
import QuerySFZD.Util

type API = Get '[HTML] (HtmlPage "index")
      :<|> "search" :> Search
      :<|> "prefer" :> ReqBody '[JSON] Prefer :> Post '[HTML] NoContent
      :<|> "static" :> Raw

type Search = QueryParam' '[Required] "backend"         Backend
           :> QueryParam' '[Required] "characters"      SearchChars
           :> QueryParams             "style"           Style
           :> QueryParam' '[Required] "author"          CalligrapherName
           :> QueryParam' '[Required] "fallbacks"       Fallbacks
           :> QueryParam' '[Optional] "skipNotFound"    SkipNotFound
           :> QueryParam' '[Optional] "saveQuery"       SaveQuery
           :> QueryParam' '[Optional] "preferredOnly"   PreferredOnly
           :> QueryParam' '[Optional] "avoidRepetition" AvoidRepetition
           :> Get '[HTML] (HtmlPage "results")

api :: Proxy API
api = Proxy

-- | Turn search query back into a URL
--
-- Useful for generating links
renderQuery :: Query -> Text
renderQuery Query{..} = toUrlPiece $
    safeLink api (Proxy :: Proxy ("search" :> Search))
      queryBackend
      querySearchChars
      queryStyles
      (fromMaybe (CalligrapherName "") queryCalligrapherName)
      queryFallbacks
      querySkipNotFound
      querySaveQuery
      queryPreferredOnly
      queryAvoidRepetition
