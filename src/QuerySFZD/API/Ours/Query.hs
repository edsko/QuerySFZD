{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module QuerySFZD.API.Ours.Query (
    SearchChar(..)
  , SearchChars(..)
  , searchCharsToString
  , Style(..)
  , styleDescription
  , styleHttpApiData
  , Author(..)
  , Fallbacks(..)
  , Query(..)
  ) where

import Codec.Serialise
import Data.Coerce (coerce)
import Servant

import qualified Data.Text as Text

import QuerySFZD.Util

-- | Characters we're seaching for
newtype SearchChar = SearchChar { searchChar :: Char }
  deriving newtype (Eq, Ord, Show)

-- | We allow to search for multiple characters at once
--
-- The order of the list here matters: we want to display the characters
-- in the order the user requested them.
newtype SearchChars = SearchChars { searchCharsToList :: [SearchChar] }
  deriving newtype (Semigroup, Monoid, Show)

searchCharsToString :: SearchChars -> String
searchCharsToString = coerce

instance FromHttpApiData SearchChars where
  parseQueryParam = Right . SearchChars . map SearchChar . Text.unpack

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

styleHttpApiData :: Style -> String
styleHttpApiData Regular     = "0"
styleHttpApiData SemiCursive = "1"
styleHttpApiData Cursive     = "2"
styleHttpApiData Clerical    = "3"
styleHttpApiData Seal        = "4"
styleHttpApiData Small       = "5"

instance FromHttpApiData Style where
  parseQueryParam "0" = Right Regular
  parseQueryParam "1" = Right SemiCursive
  parseQueryParam "2" = Right Cursive
  parseQueryParam "3" = Right Clerical
  parseQueryParam "4" = Right Seal
  parseQueryParam "5" = Right Small
  parseQueryParam p   = Left $ "Could not parse style '" <> p <> "'"

data Query = Query {
      queryChars     :: SearchChars
    , queryStyle     :: Style
    , queryAuthor    :: Maybe Author
    , queryFallbacks :: Fallbacks
    }
  deriving (Show)

-- | Author
newtype Author = Author { authorToString :: String }
  deriving newtype (Show, Eq, Ord, Serialise, FromHttpApiData)

-- | Fallbacks
newtype Fallbacks = Fallbacks [Author]
  deriving newtype (Show)

instance FromHttpApiData Fallbacks where
  parseQueryParam = Right
                  . Fallbacks
                  . map Author
                  . filter (not . null)
                  . map trim
                  . explode ','
                  . Text.unpack
