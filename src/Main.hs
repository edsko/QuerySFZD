module Main where

import           Network.HTTP.Client
import qualified Network.Wai.Handler.Warp as Wai
import           Servant

import qualified QuerySFZD.API.Ours as Ours
import           QuerySFZD.Cache
import           QuerySFZD.Server

app :: Manager -> Cache -> Application
app mgr cache = serve Ours.api $ server mgr cache

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    withCache $ \cache ->
      Wai.run 8080 $ app mgr cache
