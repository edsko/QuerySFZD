{-# LANGUAGE MultiParamTypeClasses #-}

module QuerySFZD.API.Theirs.CiDianWang.Results (
    Results(..)
  ) where

import Data.ByteString.Lazy (ByteString)

import Servant.API.ContentTypes
import Servant.HTML.Blaze

data Results = Results ByteString

instance MimeUnrender HTML Results where
  mimeUnrender _ = Right . Results
