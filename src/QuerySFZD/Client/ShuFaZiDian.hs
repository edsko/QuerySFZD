module QuerySFZD.Client.ShuFaZiDian (
    search
  ) where

import Network.HTTP.Client (Manager)
-- import Servant
import Data.List (isPrefixOf)
import Servant.Client hiding (baseUrl)
import Text.HTML.TagSoup

import qualified Data.Map.Strict as Map

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Ours.Results
import QuerySFZD.Cache
import QuerySFZD.Data.Calligraphers
import QuerySFZD.Util

{-
TagOpen "a" [
   ("rel","example_group")
  ,("href","http://119.29.99.55/高清a/晋/晋a王献之a行书a患脓帖a13_2129.jpg")
  ,("title","晋 · 王献之 · 患脓帖")
  ]

ignore

 <img src="image/wxx.png" height="250">
 <img src="shufa6/1/22bc362022f797fe96aa186d109918920.jpg" style="width:120px;height:174px">
<img src="shufa6/1/10d63bb3b64b92dfcdad3937b9906a119.jpg" style="width:120px;height:306px">

don't ignore

 <img src="高清a/宋/1/宋a蔡襄a行书a扈从帖a58_1735.jpg" height="95">
-}

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
  where
    parseTitle :: String -> Maybe (String, Maybe String)
    parseTitle title =
        case explode' " · " title of
          [_dynasty, name, source] -> Just (name, Just source)
          [_dynasty, name]         -> Just (name, Nothing)
          [name] | length name < 5 -> Just (name, Nothing)
          _otherwise               -> Nothing

parseCharacter _otherwise = Nothing

search :: Manager
       -> Cache
       -> Query
       -> IO (Either ServantError Results)
search _mgr _cache _query = do
    raw <- readFile "/home/edsko/personal/doc/calligraphy/QuerySFZD/sfzd.html"
    let soup  = parseTags raw
        chars = parseSoupWith parseCharacter soup
    return $ Right $ Results (Map.singleton (SearchChar '好') chars) (RawResult [("", parseTags raw)])
