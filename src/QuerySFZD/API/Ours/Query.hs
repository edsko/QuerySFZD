{-# LANGUAGE OverloadedStrings #-}

module QuerySFZD.API.Ours.Query (
    Query(..)
  ) where

import           Text.Blaze
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           QuerySFZD.API.Ours.Template

data Query = Query

instance ToMarkup Query where
  toMarkup Query = template $
      -- TODO: This manual encoding of the "results" URL is horrible
      H.form ! A.action "results" $ do
        H.input ! A.name "characters"
        H.input ! A.type_ "submit"
