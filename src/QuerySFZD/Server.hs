{-# LANGUAGE TypeApplications #-}

module QuerySFZD.Server (
    server
  ) where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BS
import           Data.String
import           Network.HTTP.Client (Manager)
import           Servant
import           Servant.Client
import           Servant.HTML.Blaze

import           QuerySFZD.API.Ours
import qualified QuerySFZD.API.Theirs.CiDianWang as CDW
import           QuerySFZD.API.Theirs.Common
import qualified QuerySFZD.Client.CiDianWang as CDW

server :: Manager -> Server API
server mgr =
         return IndexPage
    :<|> query mgr

query :: Manager -> Characters -> Handler Results
query mgr (Characters q) = do
{-
    let q = CDW.query
              CDW.Calligraphy
              (SingleChar (head cs))
              (Author "")
              CDW.SemiCursive
              (Just CDW.RefererSelf)
    mRes <- liftIO $ runClientM q clientEnv
    case mRes of
      Left err ->
        throwError $ err501 { errBody = fromString (renderErr err) }
      Right (CDW.Results r) -> do
        liftIO $ BS.writeFile "response" r
        return $ Results cs r
-}
    mRes <- liftIO $ mimeUnrender (Proxy @HTML) <$> BS.readFile "response.html"
    case mRes of
      Right (CDW.Results cs nextPage soup) ->
        return $ Results q ("nextPage: " ++ show nextPage ++ "\n" ++ unlines (map show soup)) cs
  where
    clientEnv :: ClientEnv
    clientEnv = mkClientEnv mgr CDW.baseUrl

    renderErr :: ServantError -> String
    renderErr err = "cidianwang.com reported error: " ++ show err
