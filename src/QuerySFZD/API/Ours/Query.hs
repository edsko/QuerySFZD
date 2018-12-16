{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module QuerySFZD.API.Ours.Query (
    SearchChar(..)
  , SearchChars(..)
  , searchCharsString
  , Style(..)
  , styleDescription
  , styleHttpApiData
  ) where

import           Data.Coerce (coerce)
import qualified Data.Text as Text

import Servant

-- | Characters we're seaching for
newtype SearchChar = SearchChar Char

-- | We allow to search for multiple characters at once
newtype SearchChars = SearchChars [SearchChar]

searchCharsString :: SearchChars -> String
searchCharsString = coerce

instance FromHttpApiData SearchChars where
  parseQueryParam = Right . SearchChars . map SearchChar . Text.unpack

data Style =
    Regular     -- ^ 楷书
  | SemiCursive -- ^ 行书
  | Cursive     -- ^ 草书
  | Clerical    -- ^ 隶书
  | Seal        -- ^ 篆书
  | Small       -- ^ 小楷
  deriving (Bounded, Enum)

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