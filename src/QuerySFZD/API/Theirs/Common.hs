{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module QuerySFZD.API.Theirs.Common (
    SingleChar(..)
  , Author(..)
  ) where

import Servant

newtype SingleChar = SingleChar Char

instance ToHttpApiData SingleChar where
  toQueryParam (SingleChar c) = toQueryParam [c]

newtype Author = Author String
  deriving newtype ToHttpApiData
