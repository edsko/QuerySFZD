{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module QuerySFZD.API.Ours.Results (
    Results(..)
  , Character(..)
  , RawResult(..)
  ) where

import           Data.Foldable (forM_)
import           Data.String (fromString)
import           Text.Blaze hiding (Tag)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.HTML.TagSoup (Tag)

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Ours.Template

data Results = Results {
      searchChars :: SearchChars
    , resultChars :: [Character]
    , rawResult   :: RawResult
    }

-- | Returned characters
data Character = Character {
      imgUrl    :: String
    , author    :: String
    , optSource :: Maybe String
    }

-- | The raw tagsoup (for debugging/development)
newtype RawResult = RawResult [Tag String]

instance ToMarkup Results where
  toMarkup Results{..} = template $ do
    H.h1 $ fromString (searchCharsString searchChars)
    forM_ resultChars $ \c -> do
      H.img ! A.src (fromString (imgUrl c))
      fromString $ author c
      forM_ (optSource c) $ \src -> do
        H.i $ fromString $ "(" ++ src ++ ")"
    H.pre $ fromString (renderRawResult rawResult)

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

renderRawResult :: RawResult -> String
renderRawResult (RawResult soup) = unlines $ map renderTag soup

renderTag :: Tag String -> String
renderTag = show . fmap DontEscape

newtype DontEscape a = DontEscape a

instance Show (DontEscape String) where
  show (DontEscape s) = "\"" ++ s ++ "\""
