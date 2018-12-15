module QuerySFZD.Server (
    server
  ) where

import Control.Monad.IO.Class
import Data.String
import Network.HTTP.Client (Manager)
import Servant
import Servant.Client

import           QuerySFZD.API.Ours
import qualified QuerySFZD.API.Theirs.CiDianWang as CiDianWang
import           QuerySFZD.API.Theirs.Common
import qualified QuerySFZD.Client.CiDianWang as CiDianWang

server :: Manager -> Server API
server mgr =
         return IndexPage
    :<|> query mgr

query :: Manager -> Characters -> Handler Results
query mgr (Characters [c]) = do
    mRes <- liftIO $ runClientM (CiDianWang.query (SingleChar c)) clientEnv
    case mRes of
      Left err ->
        throwError $ err501 { errBody = fromString (renderErr err) }
      Right (CiDianWang.Results r) ->
        return $ Results [c] r
  where
    clientEnv :: ClientEnv
    clientEnv = mkClientEnv mgr CiDianWang.baseUrl

    renderErr :: ServantError -> String
    renderErr err = "cidianwang.com reported error: " ++ show err
