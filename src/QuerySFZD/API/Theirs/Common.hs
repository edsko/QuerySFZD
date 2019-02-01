{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Data types used across all backends
module QuerySFZD.API.Theirs.Common (
    Character(..)
  ) where

import Codec.Serialise (Serialise)
import GHC.Generics (Generic)

import QuerySFZD.Data.Calligraphers (CalligrapherName)

-- | Returned characters
data Character = Character {
      charImg          :: String
    , charCalligrapher :: CalligrapherName
    , charSource       :: Maybe String
    }
  deriving stock    (Generic, Show)
  deriving anyclass Serialise
