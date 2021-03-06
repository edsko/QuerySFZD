{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module QuerySFZD.HTML.Index (renderIndex) where

import Data.Foldable (forM_)
import Data.List (intercalate)
import Data.String
import Servant
import Text.Blaze

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import QuerySFZD.API.Ours.Query
import QuerySFZD.Cache.Queries (Queries)
import QuerySFZD.Data.Calligraphers
import QuerySFZD.HTML.Template
import QuerySFZD.Util

import qualified QuerySFZD.Cache.Queries as Queries

renderIndex :: Queries -> HtmlPage "index"
renderIndex history = HtmlPage $ template True $ do
    -- TODO: This manual path (@search@) should be avoided.
    H.form ! A.action "search" $ do
      H.table $ do
        H.tr $ do
          H.td $ "Characters"
          H.td $ do
            H.input ! A.name "characters"
                    ! A.id "query"
            H.select ! A.onclick "useQuery(this.value);" $ do
              H.option ! A.value "" $ fromString "Or choose from list"
              forM_ (map searchCharsToString $ Queries.toList history) $ \sc ->
                H.option ! A.value (fromString sc) $
                  fromString sc
        H.tr $ do
          H.td $ return ()
          H.td $ do
            H.input ! A.type_ "checkbox"
                    ! A.name  "saveQuery"
            "Save query"
        H.tr $ do
          H.td $ "Style"
          H.td $
            forM_ enumerate $ \(s, checked) -> do
              if checked then
                H.input ! A.name "style"
                        ! A.type_ "checkbox"
                        ! A.value (fromText (toQueryParam s))
                        ! A.checked "checked"
              else
                H.input ! A.name "style"
                        ! A.type_ "checkbox"
                        ! A.value (fromText (toQueryParam s))
              fromString (styleDescription s)
        H.tr $ do
          H.td $ "Calligrapher"
          H.td $ do
            H.input ! A.name "author"
                    ! A.id "author"
            H.select ! A.onclick "selectAuthor(this.value);" $ do
              H.option ! A.value "" $ fromString "Or choose from list"
              forM_ knownCalligraphers $ \Calligrapher{..} ->
                H.option ! A.value (fromString cSimplified) $
                  fromString $ cSimplified ++ " " ++ cPinyin
        H.tr $ do
          H.td $ "Fallbacks"
          H.td $ do
            H.input ! A.name "fallbacks"
                    ! A.id "fallbacks"
            H.select ! A.onclick "addFallback(this.value);" $ do
              H.option ! A.value "" $ fromString "Or add from list"
              H.option ! A.value (flatten zhengkai) $ "正楷"
              H.option ! A.value (flatten zhaoti)   $ "赵体"
              forM_ knownCalligraphers $ \Calligrapher{..} ->
                H.option ! A.value (fromString cSimplified) $
                  fromString $ cSimplified ++ " " ++ cPinyin
        H.tr $ do
          H.td $ "Options"
          H.td $ do
            H.input ! A.name "skipNotFound"
                    ! A.type_ "checkbox"
            "Require all characters"
            H.br
            H.input ! A.name "avoidRepetition"
                    ! A.type_ "checkbox"
            "Avoid repeating characters"
        H.tr $ do
          H.td "Only preferred"
          H.td $ do
            forM_ [ ("", "No overlay")
                  , ("mizige", "米字格")
                  ] $ \(value, description) -> do
              H.input ! A.name "preferredOnly"
                      ! A.type_ "radio"
                      ! A.value value
              description
        H.tr $ do
          H.td "Backend"
          H.td $
            forM_ enumerate $ \(s, checked) -> do
              if checked then
                H.input ! A.name "backend"
                        ! A.type_ "radio"
                        ! A.value (fromText (toQueryParam s))
                        ! A.checked "checked"
              else
                H.input ! A.name "backend"
                        ! A.type_ "radio"
                        ! A.value (fromText (toQueryParam s))
              fromString (backendDescription s)
              H.br
        H.tr $ do
          H.td $ return ()
          H.td $ H.input ! A.type_ "submit"
  where
    flatten :: [Calligrapher] -> AttributeValue
    flatten = fromString . intercalate "," . map cSimplified

    -- All options with the first marked as such
    enumerate :: (Enum a, Bounded a) => [(a, Bool)]
    enumerate = zip [minBound .. maxBound] (True : repeat False)
