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
import QuerySFZD.Client

server :: Manager -> Server API
server mgr =
         return IndexPage
    :<|> query mgr

query :: Manager -> SearchChars -> Style -> Handler Results
query mgr qry s = do
    mRes <- liftIO $ search CiDianWang mgr qry s
    case mRes of
      Left err ->
        throwError $ err501 { errBody = fromString (renderErr err) }
      Right (cs, raw) -> do
        return $ Results qry cs raw
  where
    renderErr :: ServantError -> String
    renderErr err = "backend error: " ++ show err
