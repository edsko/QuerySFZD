-- | Abstract over the various backends
module QuerySFZD.Client (
    search
  ) where

import Network.HTTP.Client (Manager)
import Servant.Client

import QuerySFZD.API.Ours.Query
import QuerySFZD.Cache
import QuerySFZD.Client.Common

import qualified QuerySFZD.Client.CiDianWang as CDW
import qualified QuerySFZD.Client.ShuFaZiDian as SFZD

search :: Backend
       -> Manager
       -> Cache
       -> Query
       -> IO (Either ServantError Results)
search CiDianWang  = CDW.search
search ShuFaZiDian = SFZD.search
