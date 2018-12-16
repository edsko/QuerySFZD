{-# LANGUAGE OverloadedStrings #-}

module QuerySFZD.API.Ours.IndexPage (
    IndexPage(..)
  ) where

import Data.Foldable (forM_)
import Data.String
import Text.Blaze

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Ours.Template

data IndexPage = IndexPage

instance ToMarkup IndexPage where
  toMarkup IndexPage = template $
      -- TODO: This manual encoding of the "results" URL is horrible
      H.form ! A.action "search" $ do
        H.input ! A.name "characters"
        H.input ! A.type_ "submit"
        H.br
        forM_ [minBound .. maxBound] $ \s -> do
          H.input ! A.name "style"
                  ! A.type_ "radio"
                  ! A.value (fromString (styleHttpApiData s))
          fromString (styleDescription s)
