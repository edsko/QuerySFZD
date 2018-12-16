module Main where

import Network.HTTP.Client
import Servant

import qualified Network.Wai.Handler.Warp as Wai

import QuerySFZD.Cache
import QuerySFZD.Server

import qualified QuerySFZD.API.Ours as Ours

app :: Manager -> Cache -> Application
app mgr cache = serve Ours.api $ server mgr cache

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    withCache $ \cache ->
      Wai.run 8080 $ app mgr cache
