module Main where

import qualified Network.Wai.Handler.Warp as Wai
import           Servant

import qualified QuerySFZD.API.Ours       as Ours
import           QuerySFZD.Server

app :: Application
app = serve Ours.api server

main :: IO ()
main = Wai.run 8080 app
