{-# LANGUAGE RecordWildCards #-}

module QuerySFZD.API.Ours.Results (
    Results(..)
  ) where

import           Data.Foldable (forM_)
import           Data.String (fromString)
import           Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import QuerySFZD.API.Ours.Template
import QuerySFZD.API.Theirs.Common

data Results = Results {
      rawQuery   :: String
    , rawResults :: String
    , characters :: [Character]
    }

instance ToMarkup Results where
  toMarkup Results{..} = template $ do
    H.h1 $ fromString rawQuery
    forM_ characters $ \c -> do
      H.img ! A.src (fromString (imgUrl c))
      fromString $ author c
      forM_ (optSource c) $ \src -> do
        H.i $ fromString $ "(" ++ src ++ ")"
    H.pre $ fromString rawResults
