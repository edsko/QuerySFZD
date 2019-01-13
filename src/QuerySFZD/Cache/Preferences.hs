{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Preferences
--
-- Intended for qualified import
module QuerySFZD.Cache.Preferences (
    Preferences -- opaque
  , empty
  , prefer
  , sort
  ) where

import Codec.Serialise
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Ord (comparing)
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


-- | Preference
--
-- 'NoPreference' intentionally listed second so that the derived 'Ord'
-- instance sorts elements with a specified preference first.
data Preference = Preference Int | NoPreference
  deriving (Eq, Ord)

sort :: forall a. Preferences -> (a -> String) -> [a] -> [a]
sort (Preferences ps) f = sortBy (comparing g)
  where
    g :: a -> Preference
    g a = maybe NoPreference Preference $ Map.lookup (f a) ps
