{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module QuerySFZD.API.Ours.Results (
    Results(..)
  , nubResults
  , Character(..)
  , Author(..)
  , RawResult(..)
  , ResultsPage(..)
  ) where

import Codec.Serialise
import Control.Monad
import Data.Foldable (forM_)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.String (fromString)
import GHC.Generics (Generic)
import Text.Blaze hiding (Tag)
import Text.Blaze.Html5 (Html)
import Text.HTML.TagSoup (Tag)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Ours.Template

data Results = Results {
      resultChars :: Map SearchChar [Character]
    , rawResult   :: RawResult
    }

instance Semigroup Results where
  a <> b = Results {
               resultChars = combineUsing (Map.unionWith (<>)) resultChars
             , rawResult   = combineUsing (<>)                 rawResult
             }
    where
      combineUsing :: (a -> a -> a) -> (Results -> a) -> a
      combineUsing op f = f a `op` f b

instance Monoid Results where
  mempty = Results {
               resultChars = mempty
             , rawResult  =  mempty
             }

nubResults :: Results -> Results
nubResults Results{..} = Results {
      resultChars = nub <$> resultChars
    , rawResult   = rawResult
    }

-- | Returned characters
data Character = Character {
      imgUrl    :: String
    , author    :: Author
    , optSource :: Maybe String
    }
  deriving stock    (Generic, Eq)
  deriving anyclass Serialise

-- | Author
newtype Author = Author { authorToString :: String }
  deriving newtype (Eq, Ord, Serialise)

-- | The raw tagsoup (for debugging/development)
newtype RawResult = RawResult [(String, [Tag String])]
  deriving newtype (Semigroup, Monoid)

data ResultsPage = ResultsPage {
      rpSearchChars :: SearchChars
    , rpResults     :: Results
    }

instance ToMarkup ResultsPage where
  toMarkup ResultsPage{..} = template $ do
      H.h1 $ fromString $ "Search results for '"
                       ++ searchCharsToString rpSearchChars
                       ++ "'"

      forM_ (Set.toList authors) $ \(Author a) -> do
        H.h2 $ fromString $ "Calligrapher: " ++ a

        H.table ! A.class_ "characters" $ do
          H.tr $
            forM_ (searchCharsToList rpSearchChars) $ \sc ->
              H.th $ fromString [searchChar sc]
          H.tr $
            forM_ (searchCharsToList rpSearchChars) $ \sc -> do
              let matching :: [Character]
                  matching = filter ((== Author a) . author) $
                               resultChars Map.! sc
              H.td $ do
                when (null matching) $ do
                  H.img ! A.src "/static/notfound.png"
                forM_ matching $ \c -> do
                  H.img ! A.src (fromString (imgUrl c))
                  H.br

      renderRawResult rawResult
    where
      Results{..} = rpResults

      flat :: [Character]
      flat = concat (Map.elems resultChars)

      authors :: Set Author
      authors = Set.fromList $ map author flat

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
