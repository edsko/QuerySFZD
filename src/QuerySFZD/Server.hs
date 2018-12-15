module QuerySFZD.Server (
    server
  ) where

import           Servant

import           QuerySFZD.API.Ours

server :: Server API
server = return Query
    :<|> (return . Results)
