{-# LANGUAGE TypeApplications #-}

module QuerySFZD.Server (
    server
  ) where

import Control.Monad.IO.Class
import Data.String
import Network.HTTP.Client (Manager)
import Servant
import Servant.Client

import QuerySFZD.API.Ours
import QuerySFZD.Cache
import QuerySFZD.Client

server :: Manager -> Cache -> Server API
server mgr cache =
         return IndexPage
    :<|> query mgr cache
    :<|> prefer cache
    :<|> serveDirectoryWebApp "static"

query :: Manager
      -> Cache
      -> SearchChars
      -> Style
      -> Author
      -> Fallbacks
      -> Handler ResultsPage
query mgr cache sc style author fs = do
    mRes <- liftIO $ search CiDianWang mgr cache qry
    ps   <- liftIO $ getCachedPreferences cache
    case mRes of
      Right r -> return $ ResultsPage qry r ps
      Left  e -> throwError $ err501 { errBody = fromString (renderErr e) }
  where
    qry :: Query
    qry = Query {
          queryChars     = sc
        , queryStyle     = style
        , queryAuthor    = if null (authorToString author)
                             then Nothing
                             else Just author
        , queryFallbacks = fs
        }

    renderErr :: ServantError -> String
    renderErr err = "backend error: " ++ show err

prefer :: Cache -> String -> Handler NoContent
prefer cache url = do
    liftIO $ cachePreference cache url
    return NoContent
