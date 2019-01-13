{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module QuerySFZD.API.Ours.IndexPage (
    IndexPage(..)
  ) where

import Data.Foldable (forM_)
import Data.List (intercalate)
import Data.String
import Text.Blaze

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Ours.Template
import QuerySFZD.Data.Calligraphers

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
            H.td $ do
              H.input ! A.name "author"
                      ! A.id "author"
              H.select ! A.onclick "selectAuthor(this.value);" $ do
                H.option $ fromString "Or choose from list"
                forM_ calligraphers $ \Calligrapher{..} ->
                  H.option ! A.value (fromString cSimplified) $
                    fromString $ cSimplified ++ " " ++ cPinyin
          H.tr $ do
            H.td $ "Fallbacks"
            H.td $ do
              H.input ! A.name "fallbacks"
                      ! A.id "fallbacks"
              H.select ! A.onclick "addFallback(this.value);" $ do
                H.option $ fromString "Or add from list"
                H.option ! A.value (fromString zhengkai') $ "正楷"
                forM_ calligraphers $ \Calligrapher{..} ->
                  H.option ! A.value (fromString cSimplified) $
                    fromString $ cSimplified ++ " " ++ cPinyin
          H.tr $ do
            H.td $ "Options"
            H.td $ do
              H.input ! A.name "skipnotfound"
                      ! A.type_ "checkbox"
              "Require all characters"
    where
      zhengkai' :: String
      zhengkai' = intercalate "," $ map cSimplified zhengkai
