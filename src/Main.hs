module Main where

import           Network.HTTP.Client
import qualified Network.Wai.Handler.Warp as Wai
import           Servant

import qualified QuerySFZD.API.Ours as Ours
import           QuerySFZD.Server

app :: Manager -> Application
app mgr = serve Ours.api $ server mgr

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    Wai.run 8080 $ app mgr
