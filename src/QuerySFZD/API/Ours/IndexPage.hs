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
  toMarkup IndexPage = template $ do
      -- TODO: This manual path (@search@) should be avoided.
      H.form ! A.action "search" $ do
        H.table $ do
          H.tr $ do
            H.td $ "Characters"
            H.td $ do
              H.input ! A.name "characters"
              H.input ! A.type_ "submit"
          H.tr $ do
            H.td $ "Style"
            H.td $
              forM_ [minBound .. maxBound] $ \s -> do
                H.input ! A.name "style"
                        ! A.type_ "radio"
                        ! A.value (fromString (styleHttpApiData s))
                fromString (styleDescription s)
          H.tr $ do
            H.td $ "Calligrapher"
            H.td $ H.input ! A.name "author"
          H.tr $ do
            H.td $ "Fallbacks"
            H.td $ H.input ! A.name "fallbacks"
