{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Preferences
--
-- Intended for qualified import
module QuerySFZD.Preferences (
    Preferences -- opaque
  , empty
  , prefer
  ) where

import Codec.Serialise
import Data.Map.Strict (Map)
import GHC.Generics (Generic)

import qualified Data.Map.Strict as Map

-- | Character preferences
--
-- We store preferences as a map from image URLs to preference values,
-- with lower values indicating higher preference.
data Preferences = Preferences (Map String Int)
  deriving stock    Generic
  deriving anyclass Serialise

empty :: Preferences
empty = Preferences Map.empty

prefer :: String -> Preferences -> Preferences
prefer url (Preferences ps) =
      Preferences
    $ Map.insert url 0 -- this url gets highest priority
    $ fmap (+1)        -- all other urls shift up by one
    $ ps
