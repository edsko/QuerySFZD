{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuerySFZD.HTML.Results (
    ResultsPage(..)
  , renderResults
  ) where

import Control.Monad
import Data.Char (isLetter)
import Data.Foldable (forM_)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import Data.String (fromString)
import Data.Text (Text)
import Servant
import Text.Blaze hiding (Tag)
import Text.Blaze.Html5 (Html)
import Text.HTML.TagSoup (Tag)

import qualified Data.Map.Strict as Map
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import QuerySFZD.API.Ours
import QuerySFZD.API.Theirs.Common
import QuerySFZD.Cache.Preferences (Preferences)
import QuerySFZD.Client.Common
import QuerySFZD.Data.Calligraphers
import QuerySFZD.HTML.Template
import QuerySFZD.Util

import qualified QuerySFZD.Cache.Preferences as Preferences

{-------------------------------------------------------------------------------
  Record results
-------------------------------------------------------------------------------}

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
  | otherwise        = all wrote (searchCharsToList (querySearchChars rpQuery))
  where
    skipNotFound = isJust rpSkipNotFound
    Results{..}  = rpResults

    wrote :: SearchChar -> Bool
    wrote sc = any (sameCalligrapher c . charCalligrapher) (resultsChars Map.! sc)

{-------------------------------------------------------------------------------
  Top-level rendering
-------------------------------------------------------------------------------}

renderResults :: ResultsPage -> HtmlPage "results"
renderResults rp@ResultsPage{..} = HtmlPage $ do
    case (queryCalligrapherName, rpPreferredOnly) of
      (Nothing, _) ->
        template True $ do
          resultsPerCalligrapher rp
          renderRawResult resultsRaw
      (Just c, Nothing) ->
        template True $
          resultsPerCharacter rpQuery (byCharacter rp c)
      (Just c, Just overlay) ->
        template False $
          resultsPreferredOnly (byCharacter rp c) overlay
  where
    Query{queryCalligrapherName} = rpQuery
    Results{resultsRaw} = rpResults

{-------------------------------------------------------------------------------
  Default results page: show per calligrapher
-------------------------------------------------------------------------------}

resultsPerCalligrapher :: ResultsPage -> Html
resultsPerCalligrapher rp@ResultsPage{..} = do
    H.p $ fromString $ "Search results for '"
                    ++ searchCharsToString querySearchChars
                    ++ "'"
    H.p $ "Showing results for all calligraphers."

    forM_ calligraphers $ \c ->
      when (shouldIncludeCalligrapher rp c) $ do
        H.h2 $ do
          fromString $ "Calligrapher: " ++ calligrapherNameToString c ++ " "
          H.a ! A.href (fromText (urlOverlay c)) $ "(add overlay)"

        H.table ! A.class_ "characters" $ do
          H.tr $
            forM_ (searchCharsToList querySearchChars) $ \sc ->
              H.th $ fromString [searchChar sc]
          H.tr $
            forM_ (searchCharsToList querySearchChars) $ \sc -> do
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
    qry@Query{querySearchChars} = rpQuery
    Results{resultsChars} = rpResults

    flat :: [Character]
    flat = concat (Map.elems resultsChars)

    calligraphers :: [CalligrapherName]
    calligraphers = nubCalligrapherNames $ map charCalligrapher flat

    urlOverlay :: CalligrapherName -> Text
    urlOverlay name = renderQuery qry {
          queryCalligrapherName = Just name
        , queryPreferredOnly    = Just OverlayMiZiGe
        }

{-------------------------------------------------------------------------------
  Characters on the horizontal axis, all possibilities on the vertical
-------------------------------------------------------------------------------}

resultsPerCharacter :: Query -> ByCharacter -> Html
resultsPerCharacter qry ByCharacter{..} = do
    H.p $ do
      fromString $ "Search results for '"
                ++ concatMap searchCharToString (map fst bcSearchChars)
                ++ "'"
      H.br
      fromString $ "Showing results for calligrapher "
                ++ calligrapherNameToString bcPreferred ++ "."
      H.br
      if null bcFallbacks
        then "No fallbacks."
        else fromString $ "Fallbacks: " ++ renderFallbacks bcFallbacks ++ "."
      H.br
      H.a ! A.href (fromText urlOverlay) $ "Add overlay"
    H.table ! A.class_ "characters" $ do
      H.tr $
        forM_ bcSearchChars $ \(sc, _n) ->
          H.th $ fromString (searchCharToString sc)
      H.tr $
        forM_ bcSearchChars $ \(sc, n) -> do
          let matching =
               case queryAvoidRepetition qry of
                 Just AvoidRepetition -> rotate n (bcMatches Map.! sc)
                 Nothing              ->           bcMatches Map.! sc
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

    urlOverlay :: Text
    urlOverlay = renderQuery qry { queryPreferredOnly = Just OverlayMiZiGe }

{-------------------------------------------------------------------------------
  Rendering page
-------------------------------------------------------------------------------}

resultsPreferredOnly :: ByCharacter -> PreferredOnly -> Html
resultsPreferredOnly ByCharacter{..} overlay = do
    H.textarea $ return ()
    H.br
    H.div ! A.id "scroll" $ do
      imgs <- forM (zip bcSearchChars [1..]) $ \((sc, n), i :: Int) ->
          case rotate n (bcMatches Map.! sc) of
            []  -> do
              H.div ! A.style "float: left;" $ do
                H.input ! A.class_ "mizigeHeader"
                        ! A.value (fromString (searchCharToString sc))
                H.br
                H.img ! A.src "/static/notfound.png"
                      ! A.class_ "mizigeNotFound"
              return Nothing
            (ch:_) -> do
              let name = fromString $ calligrapherNameToString (charCalligrapher ch)
              H.div ! A.style "float: left;" $ do
                H.input ! A.class_ "mizigeHeader"
                        ! A.value name
                H.br
                H.canvas ! A.class_ "mizige"
                         ! A.id (fromString ("canvas" ++ show i))
                         $ return ()
              return $ Just (charImg ch)

      let arr :: String
          arr = "["
             ++ intercalate ","
                  (map (maybe "null" (\img -> ['"'] ++ img ++ ['"'])) imgs)
             ++ "]"

      H.script ! A.type_ "application/javascript" $ do
        fromString $ concat [
            "document.addEventListener('DOMContentLoaded', function() { "
          , "  addOverlays(" ++ arr
                     ++ ", " ++ show (toQueryParam overlay)
                     ++ ");"
          , "}, false);"
          ]

{-------------------------------------------------------------------------------
  Organize results by character
-------------------------------------------------------------------------------}

data ByCharacter = ByCharacter {
      bcPreferred   :: CalligrapherName
    , bcFallbacks   :: [CalligrapherName]
    , bcSearchChars :: [(SearchChar, Int)] -- ^ Marked for repetitions
    , bcMatches     :: Map SearchChar [Character]
    }

byCharacter :: ResultsPage -> CalligrapherName -> ByCharacter
byCharacter ResultsPage{..} c = ByCharacter {
      bcPreferred   = c
    , bcFallbacks   = fallbacks queryFallbacks
    , bcSearchChars = markRepetitions $ searchCharsToList querySearchChars
    , bcMatches     = Map.fromList $ map (\sc -> (sc, orderedMatches sc)) $
                        searchCharsToList querySearchChars
    }
  where
    Query{queryFallbacks, querySearchChars} = rpQuery
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
