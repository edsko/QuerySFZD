{-# LANGUAGE RecordWildCards #-}

module QuerySFZD.API.Ours.Results (
    Results(..)
  ) where

import           Data.ByteString.Lazy (ByteString)
import           Data.String (fromString)
import           Text.Blaze
import qualified Text.Blaze.Html5 as H
-- import qualified Text.Blaze.Html5.Attributes      as A
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import QuerySFZD.API.Ours.Template

data Results = Results {
      characters :: String
    , raw        :: ByteString
    }

instance ToMarkup Results where
  toMarkup Results{..} = template $ do
    H.h1 $ fromString characters
    H.pre $ fromString (UTF8.toString raw)
