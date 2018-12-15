module QuerySFZD.API.Theirs.Common (
    SingleChar(..)
  ) where

import Web.HttpApiData

newtype SingleChar = SingleChar Char

instance ToHttpApiData SingleChar where
  toQueryParam (SingleChar c) = toQueryParam [c]
