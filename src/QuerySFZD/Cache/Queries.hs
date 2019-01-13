{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module QuerySFZD.Cache.Queries (
    Queries -- opaque
  , toList
  , empty
  , insert
  ) where

import Codec.Serialise
import GHC.Generics (Generic)

import QuerySFZD.API.Ours.Query (SearchChars)

data Queries = Queries { toList :: [SearchChars] }
  deriving stock    Generic
  deriving anyclass Serialise

empty :: Queries
empty = Queries []

insert :: SearchChars -> Queries -> Queries
insert sc (Queries scs) = Queries $ sc : filter (/= sc) scs
