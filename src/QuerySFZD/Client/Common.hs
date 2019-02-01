{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module QuerySFZD.Client.Common (
    -- * Results
    Results(..)
  , nubResults
  , RawResult(..)
  ) where

import Data.Function (on)
import Data.List (nubBy)
import Data.Map.Strict (Map)
import Text.HTML.TagSoup (Tag)

import qualified Data.Map.Strict as Map

import QuerySFZD.API.Ours
import QuerySFZD.API.Theirs.Common

{-------------------------------------------------------------------------------
  Results
-------------------------------------------------------------------------------}

data Results = Results {
      resultsChars :: Map SearchChar [Character]
    , resultsRaw   :: RawResult
    }

instance Semigroup Results where
  a <> b = Results {
               resultsChars = combineUsing (Map.unionWith (<>)) resultsChars
             , resultsRaw   = combineUsing (<>)                 resultsRaw
             }
    where
      combineUsing :: (a -> a -> a) -> (Results -> a) -> a
      combineUsing op f = f a `op` f b

instance Monoid Results where
  mempty = Results {
               resultsChars = mempty
             , resultsRaw   = mempty
             }

nubResults :: Results -> Results
nubResults Results{..} = Results {
      resultsChars = nubBy ((==) `on` charImg) <$> resultsChars
    , resultsRaw   = resultsRaw
    }

-- | The raw tagsoup (for debugging/development)
newtype RawResult = RawResult [(String, [Tag String])]
  deriving newtype (Semigroup, Monoid)
