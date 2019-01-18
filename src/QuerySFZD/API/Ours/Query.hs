{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module QuerySFZD.API.Ours.Query (
    -- * Backend
    Backend(..)
  , backendDescription
  , backendHttpApiData
    -- * Query proper
  , SearchChar(..)
  , SearchChars(..)
  , searchCharToString
  , searchCharsToString
  , Style(..)
  , styleDescription
  , styleHttpApiData
  , Fallbacks(..)
  , SkipNotFound(..)
  , SaveQuery(..)
  , PreferredOnly(..)
  , preferredOverlay
  , Query(..)
  ) where

import Codec.Serialise
import Data.Coerce (coerce)
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
  deriving (Bounded, Enum)

backendDescription :: Backend -> String
backendDescription CiDianWang  = "www.cidiangwang.com"
backendDescription ShuFaZiDian = "www.shufazidian.com"

backendHttpApiData :: Backend -> String
backendHttpApiData CiDianWang  = "0"
backendHttpApiData ShuFaZiDian = "1"

instance FromHttpApiData Backend where
  parseQueryParam "0" = Right $ CiDianWang
  parseQueryParam "1" = Right $ ShuFaZiDian
  parseQueryParam str = Left $ "unexpected backend " <> str

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
      queryChars        :: SearchChars
    , queryStyle        :: Style
    , queryCalligrapher :: Maybe CalligrapherName
    , queryFallbacks    :: Fallbacks
    }
  deriving (Show)

-- | Fallbacks
newtype Fallbacks = Fallbacks { fallbacks :: [CalligrapherName] }
  deriving newtype (Show)

-- | Skip entries for authors with not-found characters
data SkipNotFound = SkipNotFound

-- | Save this query to the history
data SaveQuery = SaveQuery

-- | Only show the preferred characters
--
-- We specify which overlay we want
data PreferredOnly =
    OverlayNone
  | OverlayMiZiGe

preferredOverlay :: PreferredOnly -> String
preferredOverlay OverlayNone   = ""
preferredOverlay OverlayMiZiGe = "mizige"

instance FromHttpApiData SkipNotFound where
  parseQueryParam = const (Right SkipNotFound)

instance FromHttpApiData SaveQuery where
  parseQueryParam = const (Right SaveQuery)

instance FromHttpApiData PreferredOnly where
  parseQueryParam ""       = Right OverlayNone
  parseQueryParam "mizige" = Right OverlayMiZiGe
  parseQueryParam str      = Left ("Invalid overlay " <> str)

instance FromHttpApiData Fallbacks where
  parseQueryParam = Right
                  . Fallbacks
                  . map CalligrapherName
                  . filter (not . null)
                  . map trim
                  . explode ','
                  . Text.unpack
