{-# LANGUAGE MultiParamTypeClasses #-}

module QuerySFZD.API.Theirs.ShuFaZiDian.Results (
    SfzdResults(..)
  ) where

import Data.List (isPrefixOf)
import Servant.API.ContentTypes
import Servant.HTML.Blaze
import Text.HTML.TagSoup

import qualified Data.ByteString.Lazy.UTF8 as UTF8

import QuerySFZD.API.Theirs.Common
import QuerySFZD.Data.Calligraphers
import QuerySFZD.Util

data SfzdResults = SfzdResults {
      sfzdCharacters :: [Character]
    , sfzdRaw        :: [Tag String]
    }

instance MimeUnrender HTML SfzdResults where
  mimeUnrender _ = Right . parseSoup . parseTags . UTF8.toString

parseSoup :: [Tag String] -> SfzdResults
parseSoup soup = SfzdResults {
      sfzdCharacters = parseSoupWith parseCharacter soup
    , sfzdRaw        = soup
    }

parseCharacter :: [Tag String] -> Maybe (Character, [Tag String])
parseCharacter
    ( TagOpen "a" attrsA
    : leftover
    )
  | Just "example_group" <- findAttr "rel"   attrsA
  , Just href            <- findAttr "href"  attrsA
  , Just title           <- findAttr "title" attrsA
  , Just (name, src)     <- parseTitle title
  = Just ( Character {
             charCalligrapher = CalligrapherName name
           , charSource       = src
           , charImg          = if "http" `isPrefixOf` href
                                  then href
                                  else "http://shufazidian.com/" ++ href
            }
         , leftover
         )
parseCharacter _otherwise = Nothing

parseTitle :: String -> Maybe (String, Maybe String)
parseTitle title =
    case explode' " · " title of
      [_dynasty, name, source] -> Just (name, Just source)
      [_dynasty, name]         -> Just (name, Nothing)
      [name] | length name < 5 -> Just (name, Nothing)
      _otherwise               -> Nothing
