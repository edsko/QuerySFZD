{-# LANGUAGE OverloadedStrings #-}

module QuerySFZD.API.Ours.IndexPage (
    IndexPage(..)
  ) where

import           Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import QuerySFZD.API.Ours.Template

data IndexPage = IndexPage

instance ToMarkup IndexPage where
  toMarkup IndexPage = template $
      -- TODO: This manual encoding of the "results" URL is horrible
      H.form ! A.action "results" $ do
        H.input ! A.name "characters"
        H.input ! A.type_ "submit"
