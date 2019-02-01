{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module QuerySFZD.API.Ours.Query (
    -- * Backend
    Backend(..)
  , backendDescription
    -- * Query proper
  , SearchChar(..)
  , SearchChars(..)
  , searchCharToString
  , searchCharsToString
  , Style(..)
  , styleDescription
  , Fallbacks(..)
  , SkipNotFound(..)
  , SaveQuery(..)
  , PreferredOnly(..)
  , Query(..)
  ) where

import Codec.Serialise
import Data.Coerce (coerce)
import Data.List (intersperse)
import Data.String
import Servant

import qualified Data.Text as Text

import QuerySFZD.Data.Calligraphers
import QuerySFZD.Util

{-------------------------------------------------------------------------------
  Backend
-------------------------------------------------------------------------------}

-- | Which backend to use?
data Backend =
    CiDianWang
  | ShuFaZiDian
  deriving (Show, Bounded, Enum)

backendDescription :: Backend -> String
backendDescription CiDianWang  = "www.cidiangwang.com"
backendDescription ShuFaZiDian = "www.shufazidian.com"

instance FromHttpApiData Backend where
  parseQueryParam "0" = Right $ CiDianWang
  parseQueryParam "1" = Right $ ShuFaZiDian
  parseQueryParam str = Left $ "unexpected backend " <> str

instance ToHttpApiData Backend where
  toQueryParam CiDianWang  = "0"
  toQueryParam ShuFaZiDian = "1"

{-------------------------------------------------------------------------------
  Query proper
-------------------------------------------------------------------------------}

-- | Characters we're seaching for
newtype SearchChar = SearchChar { searchChar :: Char }
  deriving newtype (Eq, Ord, Show, Serialise)

searchCharToString :: SearchChar -> String
searchCharToString (SearchChar c) = [c]

-- | We allow to search for multiple characters at once
--
-- The order of the list here matters: we want to display the characters
-- in the order the user requested them.
newtype SearchChars = SearchChars { searchCharsToList :: [SearchChar] }
  deriving newtype (Eq, Semigroup, Monoid, Show, Serialise)

instance IsString SearchChars where
  fromString = coerce

searchCharsToString :: SearchChars -> String
searchCharsToString = coerce

instance FromHttpApiData SearchChars where
  parseQueryParam = Right . fromText

instance ToHttpApiData SearchChars where
  toQueryParam = Text.pack . searchCharsToString

{-------------------------------------------------------------------------------
  Style
-------------------------------------------------------------------------------}

data Style =
    Regular     -- ^ 楷书
  | SemiCursive -- ^ 行书
  | Cursive     -- ^ 草书
  | Clerical    -- ^ 隶书
  | Seal        -- ^ 篆书
  | Small       -- ^ 小楷
  deriving (Bounded, Enum, Show)

styleDescription :: Style -> String
styleDescription Regular     = "楷书"
styleDescription SemiCursive = "行书"
styleDescription Cursive     = "草书"
styleDescription Clerical    = "隶书"
styleDescription Seal        = "篆书"
styleDescription Small       = "小楷"

instance FromHttpApiData Style where
  parseQueryParam "0" = Right Regular
  parseQueryParam "1" = Right SemiCursive
  parseQueryParam "2" = Right Cursive
  parseQueryParam "3" = Right Clerical
  parseQueryParam "4" = Right Seal
  parseQueryParam "5" = Right Small
  parseQueryParam p   = Left $ "Could not parse style '" <> p <> "'"

instance ToHttpApiData Style where
  toQueryParam Regular     = "0"
  toQueryParam SemiCursive = "1"
  toQueryParam Cursive     = "2"
  toQueryParam Clerical    = "3"
  toQueryParam Seal        = "4"
  toQueryParam Small       = "5"

{-------------------------------------------------------------------------------
  Fallbacks
-------------------------------------------------------------------------------}

-- | Fallbacks
newtype Fallbacks = Fallbacks { fallbacks :: [CalligrapherName] }
  deriving newtype (Show)

instance FromHttpApiData Fallbacks where
  parseQueryParam = Right
                  . Fallbacks
                  . map CalligrapherName
                  . filter (not . null)
                  . map trim
                  . explode ','
                  . Text.unpack

instance ToHttpApiData Fallbacks where
  toQueryParam (Fallbacks cs) = Text.pack . mconcat $
      intersperse "," (map calligrapherNameToString cs)

{-------------------------------------------------------------------------------
  Boolean arguments
-------------------------------------------------------------------------------}

-- | Skip entries for authors with not-found characters
data SkipNotFound = SkipNotFound
  deriving (Show)

-- | Save this query to the history
data SaveQuery = SaveQuery
  deriving (Show)

instance FromHttpApiData SkipNotFound where
  parseQueryParam = const (Right SkipNotFound)

instance FromHttpApiData SaveQuery where
  parseQueryParam = const (Right SaveQuery)

instance ToHttpApiData SkipNotFound where
  toQueryParam SkipNotFound = "on"

instance ToHttpApiData SaveQuery where
  toQueryParam SaveQuery = "on"

{-------------------------------------------------------------------------------
  Only show preferred characters
-------------------------------------------------------------------------------}

-- | Only show the preferred characters
--
-- We specify which overlay we want
data PreferredOnly =
    OverlayNone
  | OverlayMiZiGe
  deriving (Show)

instance FromHttpApiData PreferredOnly where
  parseQueryParam ""       = Right OverlayNone
  parseQueryParam "mizige" = Right OverlayMiZiGe
  parseQueryParam str      = Left ("Invalid overlay " <> str)

instance ToHttpApiData PreferredOnly where
  toQueryParam OverlayNone   = ""
  toQueryParam OverlayMiZiGe = "mizige"

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Summary of all query parameters
data Query = Query {
      queryBackend          :: Backend
    , querySearchChars      :: SearchChars
    , queryStyle            :: Style
    , queryCalligrapherName :: Maybe CalligrapherName
    , queryFallbacks        :: Fallbacks
    , querySkipNotFound     :: Maybe SkipNotFound
    , querySaveQuery        :: Maybe SaveQuery
    , queryPreferredOnly    :: Maybe PreferredOnly
    }
  deriving (Show)
