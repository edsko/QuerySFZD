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
  , Query(..)
  , Referer(..)
  , Author(..)
  , module Export
  ) where

import           Data.Proxy
import qualified Data.Text as Text
import           Servant
import           Servant.HTML.Blaze

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Theirs.CiDianWang.Results as Export
import QuerySFZD.Util

{-------------------------------------------------------------------------------
  Raw API
-------------------------------------------------------------------------------}

-- | CDW search API
--
-- > http://search.cidianwang.com/?m=8&q=好&z=输入书法家&y=3
type API = Search :<|> NextPage

-- | Search for a character
type Search = QueryParam' '[Required] "m" (CDW Query)
           :> QueryParam' '[Required] "q" (CDW SearchChar)
           :> QueryParam' '[Required] "z" (CDW Author)
           :> QueryParam' '[Required] "y" (CDW Style)
           :> Header' '[] "Referer" (CDW Referer)
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
data Query = Calligraphy

-- | Author
newtype Author = Author String

-- | Who sent the request?
data Referer =
    -- | cidiangwang.com responds with a 404 if this is not set
    RefererSelf

{-------------------------------------------------------------------------------
  CDW specific unparsers
-------------------------------------------------------------------------------}

-- | Different backends require different 'ToHttpApiData' instances
--
-- For consistency we use this wrapper even for CDW specific types.
newtype CDW a = CDW a

instance ToHttpApiData (CDW SearchChar) where
  toQueryParam (CDW (SearchChar c)) = Text.pack [c]

instance ToHttpApiData (CDW Author) where
  toQueryParam (CDW (Author a)) = Text.pack a

instance ToHttpApiData (CDW Style) where
  toQueryParam (CDW SemiCursive) = "0"
  toQueryParam (CDW Regular)     = "1"
  toQueryParam (CDW Cursive)     = "2"
  toQueryParam (CDW Clerical)    = "3"
  toQueryParam (CDW Seal)        = "4"
  toQueryParam (CDW Small)       = "6"

instance ToHttpApiData (CDW Query) where
  toQueryParam (CDW Calligraphy) = "8"

instance ToHttpApiData (CDW Referer) where
  toQueryParam (CDW RefererSelf) = "http://www.cidianwang.com/shufa/"
