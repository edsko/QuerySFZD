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
import Data.Char (isLetter)
import Data.Foldable (forM_)
import Data.List (intercalate, nub)
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
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
import QuerySFZD.Preferences
import QuerySFZD.Util

import qualified QuerySFZD.Preferences as Preferences

data Results = Results {
      resultsChars :: Map SearchChar [Character]
    , resultsRaw   :: RawResult
    }

instance Semigroup Results where
  a <> b = Results {
               resultsChars = combineUsing (Map.unionWith (<>)) resultsChars
             , resultsRaw   = combineUsing (<>)                 resultsRaw
             }
    where
      combineUsing :: (a -> a -> a) -> (Results -> a) -> a
      combineUsing op f = f a `op` f b

instance Monoid Results where
  mempty = Results {
               resultsChars = mempty
             , resultsRaw   = mempty
             }

nubResults :: Results -> Results
nubResults Results{..} = Results {
      resultsChars = nub <$> resultsChars
    , resultsRaw   = resultsRaw
    }

-- | Returned characters
data Character = Character {
      charImg    :: String
    , charAuthor :: Author
    , charSource :: Maybe String
    }
  deriving stock    (Generic, Eq)
  deriving anyclass Serialise

-- | The raw tagsoup (for debugging/development)
newtype RawResult = RawResult [(String, [Tag String])]
  deriving newtype (Semigroup, Monoid)

data ResultsPage = ResultsPage {
      rpQuery        :: Query
    , rpResults      :: Results
    , rpPreferences  :: Preferences
    , rpSkipNotFound :: Maybe SkipNotFound
    }

shouldIncludeAuthor :: ResultsPage -> Author -> Bool
shouldIncludeAuthor ResultsPage{..} a
  | not skipNotFound = True
  | otherwise        = all wrote (searchCharsToList (queryChars rpQuery))
  where
    skipNotFound = isJust rpSkipNotFound
    Results{..}  = rpResults

    wrote :: SearchChar -> Bool
    wrote sc = any ((== a) . charAuthor) (resultsChars Map.! sc)

instance ToMarkup ResultsPage where
  toMarkup rp@ResultsPage{..} = template $ do
      H.p $ fromString $ "Search results for '"
                      ++ searchCharsToString queryChars
                      ++ "'"

      case queryAuthor of
        Nothing -> do
          H.p "Showing results for all calligraphers."

          forM_ (Set.toList authors) $ \author ->
            when (shouldIncludeAuthor rp author) $ do
              H.h2 $ fromString $ "Calligrapher: " ++ authorToString author

              H.table ! A.class_ "characters" $ do
                H.tr $
                  forM_ (searchCharsToList queryChars) $ \sc ->
                    H.th $ fromString [searchChar sc]
                H.tr $
                  forM_ (searchCharsToList queryChars) $ \sc -> do
                    let matchesAuthor :: Character -> Bool
                        matchesAuthor c = charAuthor c == author

                        matching :: [Character]
                        matching = filter matchesAuthor $ resultsChars Map.! sc

                    H.td $ do
                      when (null matching && isLetter (searchChar sc)) $ do
                        H.img ! A.src "/static/notfound.png"
                      forM_ matching $ \c -> do
                        H.img ! A.src (fromString (charImg c))
                        H.br
                        case charSource c of
                          Nothing  -> "(Unknown source)"
                          Just src -> fromString $ "(" ++ src ++ ")"
                        H.br

        Just (Author a) -> do
          H.p $ do
            fromString $ "Showing results for calligrapher " ++ a ++ "."
            H.br
            if null (fallbacks queryFallbacks)
              then "No fallbacks."
              else fromString $ "Fallbacks: " ++ renderFallbacks queryFallbacks ++ "."

            H.table ! A.class_ "characters" $ do
              H.tr $
                forM_ (searchCharsToList queryChars) $ \sc ->
                  H.th $ fromString [searchChar sc]
              H.tr $
                forM_ (searchCharsToList queryChars) $ \sc -> do
                  let matching :: [Character]
                      matching = Preferences.sort rpPreferences charImg
                               . indexInOrder
                                   ( (\c -> charAuthor c == Author a)
                                   : map (\fb c -> charAuthor c == fb)
                                         (fallbacks queryFallbacks)
                                   )
                               $ resultsChars Map.! sc

                  H.td $ do
                    when (null matching && isLetter (searchChar sc)) $ do
                      H.img ! A.src "/static/notfound.png"
                    forM_ matching $ \c -> do
                      H.img ! A.src (fromString (charImg c))
                            ! A.onclick (fromString ("prefer(\"" ++ charImg c ++ "\");"))
                      H.br
                      fromString $ authorToString (charAuthor c)
                      forM_ (charSource c) $ \src ->
                        fromString $ " (" ++ src ++ ")"
                      H.br

      renderRawResult resultsRaw
    where
      Query{..}   = rpQuery
      Results{..} = rpResults

      flat :: [Character]
      flat = concat (Map.elems resultsChars)

      authors :: Set Author
      authors = Set.fromList $ map charAuthor flat

      renderFallbacks :: Fallbacks -> String
      renderFallbacks = intercalate ", " . map authorToString . fallbacks

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
