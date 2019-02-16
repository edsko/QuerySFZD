{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module QuerySFZD.Server (
    server
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Data.String
import Network.HTTP.Client (Manager)
import Servant
import Servant.Client

import QuerySFZD.API.Ours
import QuerySFZD.Cache
import QuerySFZD.Cache.Preferences (Prefer)
import QuerySFZD.Client
import QuerySFZD.Data.Calligraphers
import QuerySFZD.HTML.Index
import QuerySFZD.HTML.Results
import QuerySFZD.Util

server :: Manager -> Cache -> Server API
server mgr cache =
         presentIndexPage cache
    :<|> query mgr cache
    :<|> prefer cache
    :<|> serveDirectoryWebApp "static"

presentIndexPage :: Cache -> Handler (HtmlPage "index")
presentIndexPage cache = do
    queries <- liftIO $ getCachedQueries cache
    return $ renderIndex queries

query :: Manager
      -> Cache
      -> Backend
      -> SearchChars
      -> [Style]
      -> CalligrapherName
      -> Fallbacks
      -> Maybe SkipNotFound
      -> Maybe SaveQuery
      -> Maybe PreferredOnly
      -> Maybe AvoidRepetition
      -> Handler (HtmlPage "results")
query mgr cache backend sc styles author fs skip save only rep = do
    liftIO $ when (isJust save) $ cacheQuery cache sc
    mRes <- liftIO $ search backend mgr cache qry
    ps   <- liftIO $ getCachedPreferences cache
    case mRes of
      Right r -> return $ renderResults $ ResultsPage qry r ps skip only
      Left  e -> throwError $ err501 { errBody = fromString (renderErr e) }
  where
    qry :: Query
    qry = Query {
          queryBackend          = backend
        , querySearchChars      = sc
        , queryStyles           = styles
        , queryCalligrapherName = if null (calligrapherNameToString author)
                                    then Nothing
                                    else Just author
        , queryFallbacks        = fs
        , querySkipNotFound     = skip
        , querySaveQuery        = save
        , queryPreferredOnly    = only
        , queryAvoidRepetition  = rep
        }

    renderErr :: ServantError -> String
    renderErr err = "backend error: " ++ show err

prefer :: Cache -> Prefer -> Handler NoContent
prefer cache url = do
    liftIO $ cachePreference cache url
    return NoContent
