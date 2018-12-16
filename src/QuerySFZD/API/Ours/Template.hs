{-# LANGUAGE OverloadedStrings #-}

module QuerySFZD.API.Ours.Template (
    template
  ) where

import Text.Blaze
import Text.Blaze.Html5 (Html)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

template :: Html -> Html
template body =
    H.docTypeHtml $ do
      H.head $
        H.link ! A.rel "stylesheet"
               ! A.href "static/style.css"
      H.body $ do
        H.h1 $ H.a ! A.href "/" $ "Search 书法字典"
        body
