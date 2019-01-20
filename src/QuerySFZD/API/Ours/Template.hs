{-# LANGUAGE OverloadedStrings #-}

module QuerySFZD.API.Ours.Template (
    template
  ) where

import Control.Monad
import Text.Blaze
import Text.Blaze.Html5 (Html)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

template :: Bool -> Html -> Html
template includeTitle body =
    H.docTypeHtml $ do
      H.head $ do
        H.link ! A.rel "stylesheet"
               ! A.href "static/style.css"
        H.script ! A.type_ "application/javascript"
                 ! A.src "static/handlers.js"
                 $ return ()
      H.body $ do
        when includeTitle $ H.h1 $ H.a ! A.href "/" $ "Search 书法字典"
        body
