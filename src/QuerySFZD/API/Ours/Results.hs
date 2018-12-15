{-# LANGUAGE RecordWildCards #-}

module QuerySFZD.API.Ours.Results (
    Results(..)
  ) where

import           Data.String                 (fromString)
import           Text.Blaze
import qualified Text.Blaze.Html5            as H
-- import qualified Text.Blaze.Html5.Attributes      as A

import           QuerySFZD.API.Ours.Template

data Results = Results {
      characters :: String
    }

instance ToMarkup Results where
  toMarkup Results{..} = template $
    H.h1 $ fromString characters
