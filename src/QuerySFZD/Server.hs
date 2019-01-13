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
import QuerySFZD.Client
import QuerySFZD.Data.Calligraphers

server :: Manager -> Cache -> Server API
server mgr cache =
         presentIndexPage cache
    :<|> query mgr cache
    :<|> prefer cache
    :<|> serveDirectoryWebApp "static"

presentIndexPage :: Cache -> Handler IndexPage
presentIndexPage cache = do
    queries <- liftIO $ getCachedQueries cache
    return $ IndexPage queries

query :: Manager
      -> Cache
      -> SearchChars
      -> Style
      -> CalligrapherName
      -> Fallbacks
      -> Maybe SkipNotFound
      -> Maybe SaveQuery
      -> Handler ResultsPage
query mgr cache sc style author fs skip save = do
    liftIO $ when (isJust save) $ cacheQuery cache sc
    mRes <- liftIO $ search CiDianWang mgr cache qry
    ps   <- liftIO $ getCachedPreferences cache
    case mRes of
      Right r -> return $ ResultsPage qry r ps skip
      Left  e -> throwError $ err501 { errBody = fromString (renderErr e) }
  where
    qry :: Query
    qry = Query {
          queryChars        = sc
        , queryStyle        = style
        , queryCalligrapher = if null (calligrapherNameToString author)
                               then Nothing
                               else Just author
        , queryFallbacks    = fs
        }

    renderErr :: ServantError -> String
    renderErr err = "backend error: " ++ show err

prefer :: Cache -> String -> Handler NoContent
prefer cache url = do
    liftIO $ cachePreference cache url
    return NoContent
