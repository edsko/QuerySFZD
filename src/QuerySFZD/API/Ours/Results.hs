{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module QuerySFZD.API.Ours.Results (
    Results(..)
  , Character(..)
  , Author(..)
  , RawResult(..)
  ) where

import Codec.Serialise
import Data.Foldable (forM_)
import Data.Map.Strict (Map)
import Data.String (fromString)
import GHC.Generics (Generic)
import Text.Blaze hiding (Tag)
import Text.Blaze.Html5 (Html)
import Text.HTML.TagSoup (Tag)

import qualified Data.Map.Strict as Map
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Ours.Template

data Results = Results {
      searchChars :: SearchChars
    , resultChars :: Map SearchChar [Character]
    , rawResult   :: RawResult
    }

instance Semigroup Results where
  a <> b = Results {
               searchChars = combineUsing (<>)                 searchChars
             , resultChars = combineUsing (Map.unionWith (<>)) resultChars
             , rawResult   = combineUsing (<>)                 rawResult
             }
    where
      combineUsing :: (a -> a -> a) -> (Results -> a) -> a
      combineUsing op f = f a `op` f b

instance Monoid Results where
  mempty = Results {
               searchChars = mempty
             , resultChars = mempty
             , rawResult  =  mempty
             }

-- | Returned characters
data Character = Character {
      imgUrl    :: String
    , author    :: Author
    , optSource :: Maybe String
    }
  deriving stock    Generic
  deriving anyclass Serialise

-- | Author
newtype Author = Author { authorToString :: String }
  deriving newtype (Eq, Ord, Serialise)

-- | The raw tagsoup (for debugging/development)
newtype RawResult = RawResult [(String, [Tag String])]
  deriving newtype (Semigroup, Monoid)

instance ToMarkup Results where
  toMarkup Results{..} = template $ do
      H.h1 $ fromString (searchCharsToString searchChars)

      forM_ flat $ \c -> do
        H.img ! A.src (fromString (imgUrl c))
        fromString $ authorToString (author c)
        forM_ (optSource c) $ \src -> do
          H.i $ fromString $ "(" ++ src ++ ")"

      renderRawResult rawResult
    where
      flat :: [Character]
      flat = concat (Map.elems resultChars)

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

renderRawResult :: RawResult -> Html
renderRawResult (RawResult results) =
    forM_ results $ \(query, soup) -> do
      H.h2 $ fromString query
      H.pre $ fromString $ unlines $ map renderTag soup

renderTag :: Tag String -> String
renderTag = show . fmap DontEscape

newtype DontEscape a = DontEscape a

instance Show (DontEscape String) where
  show (DontEscape s) = "\"" ++ s ++ "\""
