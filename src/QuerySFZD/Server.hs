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

query :: Manager -> Cache -> SearchChars -> Style -> Handler Results
query mgr cache qry s = do
    mRes <- liftIO $ search CiDianWang mgr cache s qry
    case mRes of
      Right r -> return r
      Left  e -> throwError $ err501 { errBody = fromString (renderErr e) }
  where
    renderErr :: ServantError -> String
    renderErr err = "backend error: " ++ show err
