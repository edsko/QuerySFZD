{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module QuerySFZD.API.Ours.Results (
    Results(..)
  , nubResults
  , Character(..)
  , RawResult(..)
  , ResultsPage(..)
  ) where

import Codec.Serialise
import Control.Monad
import Data.Char (isLetter)
import Data.Foldable (forM_)
import Data.Function (on)
import Data.List (intercalate, nubBy)
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
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
import QuerySFZD.Cache.Preferences (Preferences)
import QuerySFZD.Data.Calligraphers
import QuerySFZD.Util

import qualified QuerySFZD.Cache.Preferences as Preferences

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
      resultsChars = nubBy ((==) `on` charImg) <$> resultsChars
    , resultsRaw   = resultsRaw
    }

-- | Returned characters
data Character = Character {
      charImg          :: String
    , charCalligrapher :: CalligrapherName
    , charSource       :: Maybe String
    }
  deriving stock    (Generic, Show)
  deriving anyclass Serialise

-- | The raw tagsoup (for debugging/development)
newtype RawResult = RawResult [(String, [Tag String])]
  deriving newtype (Semigroup, Monoid)

data ResultsPage = ResultsPage {
      rpQuery         :: Query
    , rpResults       :: Results
    , rpPreferences   :: Preferences
    , rpSkipNotFound  :: Maybe SkipNotFound
    , rpPreferredOnly :: Maybe PreferredOnly
    }

shouldIncludeCalligrapher :: ResultsPage -> CalligrapherName -> Bool
shouldIncludeCalligrapher ResultsPage{..} c
  | not skipNotFound = True
  | otherwise        = all wrote (searchCharsToList (queryChars rpQuery))
  where
    skipNotFound = isJust rpSkipNotFound
    Results{..}  = rpResults

    wrote :: SearchChar -> Bool
    wrote sc = any (sameCalligrapher c . charCalligrapher) (resultsChars Map.! sc)

instance ToMarkup ResultsPage where
  toMarkup rp@ResultsPage{..} = do
      case (queryCalligrapher, rpPreferredOnly) of
        (Nothing, _) ->
          template True $ do
            resultsPerCalligrapher rp
            renderRawResult resultsRaw
        (Just c, Nothing) ->
          template True $
            resultsPerCharacter  (byCharacter rp c)
        (Just c, Just overlay) ->
          template False $
            resultsPreferredOnly (byCharacter rp c) overlay
    where
      Query{queryCalligrapher} = rpQuery
      Results{resultsRaw} = rpResults

{-------------------------------------------------------------------------------
  Default results page: show per calligrapher
-------------------------------------------------------------------------------}

resultsPerCalligrapher :: ResultsPage -> Html
resultsPerCalligrapher rp@ResultsPage{..} = do
    H.p $ fromString $ "Search results for '"
                    ++ searchCharsToString queryChars
                    ++ "'"
    H.p $ "Showing results for all calligraphers."

    forM_ calligraphers $ \c ->
      when (shouldIncludeCalligrapher rp c) $ do
        H.h2 $ fromString $ "Calligrapher: " ++ calligrapherNameToString c

        H.table ! A.class_ "characters" $ do
          H.tr $
            forM_ (searchCharsToList queryChars) $ \sc ->
              H.th $ fromString [searchChar sc]
          H.tr $
            forM_ (searchCharsToList queryChars) $ \sc -> do
              let matchesAuthor :: Character -> Bool
                  matchesAuthor ch = sameCalligrapher c (charCalligrapher ch)

                  matching :: [Character]
                  matching = filter matchesAuthor $ resultsChars Map.! sc

              H.td $ do
                when (null matching && isLetter (searchChar sc)) $ do
                  H.img ! A.src "/static/notfound.png"
                forM_ matching $ \ch -> do
                  H.img ! A.src (fromString (charImg ch))
                  H.br
                  case charSource ch of
                    Nothing  -> "(Unknown source)"
                    Just src -> fromString $ "(" ++ src ++ ")"
                  H.br
  where
    Query{queryChars} = rpQuery
    Results{resultsChars} = rpResults

    flat :: [Character]
    flat = concat (Map.elems resultsChars)

    calligraphers :: [CalligrapherName]
    calligraphers = nubCalligrapherNames $ map charCalligrapher flat

{-------------------------------------------------------------------------------
  Characters on the horizontal axis, all possibilities on the vertical
-------------------------------------------------------------------------------}

resultsPerCharacter :: ByCharacter -> Html
resultsPerCharacter ByCharacter{..} = do
    H.p $ fromString $ "Search results for '"
                    ++ concatMap searchCharToString bcSearchChars
                    ++ "'"
    H.p $ do
      fromString $ "Showing results for calligrapher "
                ++ calligrapherNameToString bcPreferred ++ "."
      H.br
      if null bcFallbacks
        then "No fallbacks."
        else fromString $ "Fallbacks: " ++ renderFallbacks bcFallbacks ++ "."

      H.table ! A.class_ "characters" $ do
        H.tr $
          forM_ bcSearchChars $ \sc ->
            H.th $ fromString (searchCharToString sc)
        H.tr $
          forM_ bcSearchChars $ \sc -> do
            let matching = bcMatches Map.! sc
            H.td $ do
              when (null matching && isLetter (searchChar sc)) $ do
                H.img ! A.src "/static/notfound.png"
              forM_ matching $ \ch -> do
                H.img ! A.src (fromString (charImg ch))
                      ! A.onclick (fromString ("prefer(\"" ++ charImg ch ++ "\");"))
                H.br
                fromString $ calligrapherNameToString (charCalligrapher ch)
                forM_ (charSource ch) $ \src ->
                  fromString $ " (" ++ src ++ ")"
                H.br
  where
    renderFallbacks :: [CalligrapherName] -> String
    renderFallbacks = intercalate ", " . map calligrapherNameToString

{-------------------------------------------------------------------------------
  Rendering page
-------------------------------------------------------------------------------}

resultsPreferredOnly :: ByCharacter -> PreferredOnly -> Html
resultsPreferredOnly ByCharacter{..} overlay = do
    H.textarea $ return ()
    H.br

    calligraphers <- forM (zip bcSearchChars [1..]) $ \(sc, i :: Int) ->
        case bcMatches Map.! sc of
          []  -> do
            H.div ! A.style "float: left;" $ do
              H.input ! A.class_ "mizigeHeader"
                      ! A.value (fromString (searchCharToString sc))
              H.br
              H.img ! A.src "/static/notfound.png"
                    ! A.class_ "mizige"
            return Nothing
          (ch:_) -> do
            let bg   = "background-image: url(\""
                    ++ fromString (charImg ch)
                    ++ "\")"
                name = fromString $ calligrapherNameToString (charCalligrapher ch)
            H.div ! A.style "float: left;" $ do
              H.input ! A.class_ "mizigeHeader"
                      ! A.value name
              H.br
              H.canvas ! A.style (fromString bg)
                       ! A.class_ "mizige"
                       ! A.id (fromString ("canvas" ++ show i))
                       $ return ()
            return $ Just (charCalligrapher ch)

    let arr :: String
        arr = "["
           ++ intercalate ","
                (map (maybe "null" (\c -> ['"'] ++ calligrapherNameToString c ++ ['"']))
                     calligraphers)
           ++ "]"

    H.script ! A.type_ "application/javascript" $ do
      fromString $ concat [
          "document.addEventListener('DOMContentLoaded', function() { "
        , "  addOverlays(" ++ arr
                   ++ ", " ++ show (preferredOverlay overlay)
                   ++ ");"
        , "}, false);"
        ]

{-------------------------------------------------------------------------------
  Organize results by character
-------------------------------------------------------------------------------}

data ByCharacter = ByCharacter {
      bcPreferred   :: CalligrapherName
    , bcFallbacks   :: [CalligrapherName]
    , bcSearchChars :: [SearchChar]
    , bcMatches     :: Map SearchChar [Character]
    }

byCharacter :: ResultsPage -> CalligrapherName -> ByCharacter
byCharacter ResultsPage{..} c = ByCharacter {
      bcPreferred   = c
    , bcFallbacks   = fallbacks queryFallbacks
    , bcSearchChars = searchCharsToList queryChars
    , bcMatches     = Map.fromList $ map (\sc -> (sc, orderedMatches sc)) $
                        searchCharsToList queryChars
    }
  where
    Query{queryFallbacks, queryChars} = rpQuery
    Results{resultsChars} = rpResults

    orderedMatches :: SearchChar -> [Character]
    orderedMatches sc =
          Preferences.sort rpPreferences charImg
        . indexInOrder
            ( (\ch -> sameCalligrapher c (charCalligrapher ch))
            : map (\fb ch -> sameCalligrapher fb (charCalligrapher ch))
                  (filter (not . sameCalligrapher c) $ fallbacks queryFallbacks)
            )
        $ resultsChars Map.! sc

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
