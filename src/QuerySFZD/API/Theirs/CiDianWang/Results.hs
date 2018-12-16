{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module QuerySFZD.API.Theirs.CiDianWang.Results (
    CdwResults(..)
  ) where

import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.List (find, isPrefixOf)
import           Data.Maybe (listToMaybe)
import           Data.String
import           Servant.API.ContentTypes
import           Servant.HTML.Blaze
import           Text.HTML.TagSoup

import QuerySFZD.API.Ours.Results
import QuerySFZD.Util

data CdwResults = CdwResults {
      cdwCharacters :: [Character]
    , cdwNextPage   :: Maybe DynPath
    , cdwRaw        :: [Tag String]
    }

instance MimeUnrender HTML CdwResults where
  mimeUnrender _ = Right . parseSoup . parseTags . UTF8.toString

parseSoup :: [Tag String] -> CdwResults
parseSoup soup = CdwResults {
      cdwCharacters = parseSoupWith parseCharacter soup
    , cdwNextPage   = listToMaybe $ parseSoupWith parseNextPage soup
    , cdwRaw        = soup
    }

parseCharacter :: [Tag String] -> Maybe (Character, [Tag String])
parseCharacter
    ( TagOpen "img" attrsImg
    : TagText " "
    : TagOpen "a" _attrsA
    : TagText author
    : TagClose "a"
    : TagOpen "br" []
    : TagOpen "span" _attrsSpan
    : TagText ('出' : '自' : '：' : source)
    : leftover
    )
  | Just _           <- find isBoxShow attrsImg
  , Just (_, imgUrl) <- find (isAttr "src") attrsImg
  = Just (Character {..}, leftover)
  where
    optSource = Just source
parseCharacter
    ( TagOpen "img" attrsImg
    : TagText " "
    : TagOpen "a" _attrsA
    : TagText author
    : TagClose "a"
    : leftover
    )
  | Just _           <- find isBoxShow attrsImg
  , Just (_, imgUrl) <- find (isAttr "src") attrsImg
  = Just (Character {..}, leftover)
  where
    optSource = Nothing
parseCharacter _otherwise = Nothing

parseNextPage :: [Tag String] -> Maybe (DynPath, [Tag String])
parseNextPage
    ( TagOpen "a" attrsA
    : TagText "下一页"
    : TagClose "a"
    : leftover
    )
  | Just (_, nextPage) <- find (isAttr "href") attrsA
  = Just (fromString nextPage, leftover)
parseNextPage _otherwose = Nothing

{-------------------------------------------------------------------------------
  Auxiliary: attribute parsers
-------------------------------------------------------------------------------}

isBoxShow :: Attribute String -> Bool
isBoxShow ("onClick", val) = "box.Show()" `isPrefixOf` val
isBoxShow _                = False

isAttr :: String -> Attribute String -> Bool
isAttr attr (attr', _) = attr == attr'
