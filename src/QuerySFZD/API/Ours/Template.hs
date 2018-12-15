module QuerySFZD.API.Ours.Template (
    template
  ) where

import           Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
--import qualified Text.Blaze.Html5.Attributes as A

template :: Html -> Html
template body =
    H.docTypeHtml $
      H.body body
