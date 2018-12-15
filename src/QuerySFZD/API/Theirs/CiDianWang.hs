{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module QuerySFZD.API.Theirs.CiDianWang (
    API
  , Query(..)
  , Style(..)
  , Referer(..)
  , api
  , module Export
  ) where

import Data.Proxy
import Servant
import Servant.HTML.Blaze

import QuerySFZD.API.Theirs.CiDianWang.Results as Export
import QuerySFZD.API.Theirs.Common


-- > http://search.cidianwang.com/?m=8&q=好&z=输入书法家&y=3
type API = QueryParam' '[Required] "m" Query
        :> QueryParam' '[Required] "q" SingleChar
        :> QueryParam' '[Required] "z" Author
        :> QueryParam' '[Required] "y" Style
        :> Header' '[] "Referer" Referer
        :> Get '[HTML] Results

data Query = Calligraphy

instance ToHttpApiData Query where
  toQueryParam Calligraphy = "8"

data Referer =
    -- | cidiangwang.com responds with a 404 if this is not set
    RefererSelf

instance ToHttpApiData Referer where
  toQueryParam RefererSelf = "http://www.cidianwang.com/shufa/"

api :: Proxy API
api = Proxy

data Style =
    AllStyles   -- ^ 不限
  | SemiCursive -- ^ 行书
  | Regular     -- ^ 楷书
  | Cursive     -- ^ 草书
  | Clerical    -- ^ 隶书
  | Seal        -- ^ 篆书
  | Small       -- ^ 小楷

instance ToHttpApiData Style where
  toQueryParam AllStyles   = "5"
  toQueryParam SemiCursive = "0"
  toQueryParam Regular     = "1"
  toQueryParam Cursive     = "2"
  toQueryParam Clerical    = "3"
  toQueryParam Seal        = "4"
  toQueryParam Small       = "6"
