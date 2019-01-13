{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module QuerySFZD.API.Theirs.CiDianWang (
    API
  , api
  , CDW(..)
    -- * CDW specific types
  , CdwQuery(..)
  , CdwReferer(..)
  , module Export
  ) where

import Data.Proxy
import Servant
import Servant.HTML.Blaze

import qualified Data.Text as Text

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Theirs.CiDianWang.Results as Export
import QuerySFZD.Data.Calligraphers
import QuerySFZD.Util

{-------------------------------------------------------------------------------
  Raw API
-------------------------------------------------------------------------------}

-- | CDW search API
--
-- > http://search.cidianwang.com/?m=8&q=好&z=输入书法家&y=3
type API = Search :<|> NextPage

-- | Search for a character
type Search = QueryParam' '[Required] "m" CdwQuery
           :> QueryParam' '[Required] "q" (CDW SearchChar)
           :> QueryParam' '[Required] "z" (CDW CalligrapherName)
           :> QueryParam' '[Required] "y" (CDW Style)
           :> Header' '[] "Referer" CdwReferer
           :> Get '[HTML] CdwResults

-- | Get specific results page
type NextPage = DynPath :> Get '[HTML] CdwResults

api :: Proxy API
api = Proxy

{-------------------------------------------------------------------------------
  CDW specific types
-------------------------------------------------------------------------------}

-- | What are we querying?
--
-- CDW supports more than just calligraphy (but we don't, for now).
data CdwQuery = CdwCalligraphy

-- | Who sent the request?
data CdwReferer =
    -- | cidiangwang.com responds with a 404 if this is not set
    CdwRefererSelf

{-------------------------------------------------------------------------------
  CDW specific unparsers
-------------------------------------------------------------------------------}

-- | Different backends require different 'ToHttpApiData' instances
newtype CDW a = CDW a

instance ToHttpApiData (CDW SearchChar) where
  toQueryParam (CDW (SearchChar c)) = Text.pack [c]

instance ToHttpApiData (CDW CalligrapherName) where
  toQueryParam (CDW (CalligrapherName a)) = Text.pack a

instance ToHttpApiData (CDW Style) where
  toQueryParam (CDW SemiCursive) = "0"
  toQueryParam (CDW Regular)     = "1"
  toQueryParam (CDW Cursive)     = "2"
  toQueryParam (CDW Clerical)    = "3"
  toQueryParam (CDW Seal)        = "4"
  toQueryParam (CDW Small)       = "6"

instance ToHttpApiData CdwQuery where
  toQueryParam CdwCalligraphy = "8"

instance ToHttpApiData CdwReferer where
  toQueryParam CdwRefererSelf = "http://www.cidianwang.com/shufa/"
