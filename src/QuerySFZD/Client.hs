-- | Abstract over the various backends
module QuerySFZD.Client (
    Backend(..)
  , search
  ) where

import Network.HTTP.Client (Manager)
import Servant.Client

import           QuerySFZD.API.Ours.Query
import           QuerySFZD.API.Ours.Results
import           QuerySFZD.Cache
import qualified QuerySFZD.Client.CiDianWang as CDW

-- | Which backend to use?
data Backend =
    CiDianWang

search :: Backend
       -> Manager
       -> Cache
       -> SearchChars
       -> Style
       -> IO (Either ServantError ([Character], RawResult))
search CiDianWang = CDW.search
