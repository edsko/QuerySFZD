{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module QuerySFZD.API.Theirs.CiDianWang.Results (
    CdwResults(..)
  , CdwNext(..)
  , cdwNextToString
  , cdwNextFromString
  ) where

import Data.List (find, isPrefixOf)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Servant
import Servant.HTML.Blaze
import Text.HTML.TagSoup

import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Text as Text

import QuerySFZD.API.Theirs.Common
import QuerySFZD.Data.Calligraphers
import QuerySFZD.Util

-- | cidianwang.com splits results up into separate pages
newtype CdwNext = CdwNext { cdwNextToText :: Text }
  deriving newtype (ToHttpApiData)

cdwNextToString :: CdwNext -> String
cdwNextToString = Text.unpack . cdwNextToText

cdwNextFromString :: String -> CdwNext
cdwNextFromString = CdwNext . Text.pack

data CdwResults = CdwResults {
      cdwCharacters :: [Character]
    , cdwNextPage   :: Maybe CdwNext
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
    : TagText calligrapherName
    : TagClose "a"
    : TagOpen "br" []
    : TagOpen "span" _attrsSpan
    : TagText ('出' : '自' : '：' : source)
    : leftover
    )
  | Just _   <- find isBoxShow attrsImg
  , Just img <- findAttr "src" attrsImg
  = Just ( Character {
               charCalligrapher = CalligrapherName calligrapherName
             , charSource       = Just source
             , charImg          = img
             }
         , leftover
         )
parseCharacter
    ( TagOpen "img" attrsImg
    : TagText " "
    : TagOpen "a" _attrsA
    : TagText calligrapherName
    : TagClose "a"
    : leftover
    )
  | Just _   <- find isBoxShow attrsImg
  , Just img <- findAttr "src" attrsImg
  = Just ( Character {
               charCalligrapher = CalligrapherName calligrapherName
             , charSource       = Nothing
             , charImg          = img
             }
         , leftover
         )
parseCharacter _otherwise = Nothing

parseNextPage :: [Tag String] -> Maybe (CdwNext, [Tag String])
parseNextPage
    ( TagOpen "a" attrsA
    : TagText "下一页"
    : TagClose "a"
    : leftover
    )
  | Just nextPage <- findAttr "href" attrsA
  = Just (cdwNextFromString nextPage, leftover)
parseNextPage _otherwose = Nothing

{-------------------------------------------------------------------------------
  Auxiliary: attribute parsers
-------------------------------------------------------------------------------}

isBoxShow :: Attribute String -> Bool
isBoxShow ("onClick", val) = "box.Show()" `isPrefixOf` val
isBoxShow _                = False
