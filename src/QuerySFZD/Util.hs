{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module QuerySFZD.Util (
    -- * Misc
    fromText
    -- * Lists
  , partitionOn
  , explode
  , explodeText
  , explode'
  , trim
  , indexInOrder
    -- * Tagsoup
  , findAttr
  , parseSoupWith
    -- * Servant
  , HtmlPage(..)
  ) where

import Data.Char (isSpace)
import Data.List (find, stripPrefix)
import Data.Map.Strict (Map)
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.TypeLits (Symbol)
import Text.Blaze (ToMarkup (..))
import Text.Blaze.Html5 (Html)
import Text.HTML.TagSoup (Attribute)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

{-------------------------------------------------------------------------------
  Misc
-------------------------------------------------------------------------------}

fromText :: IsString a => Text -> a
fromText = fromString . Text.unpack

{-------------------------------------------------------------------------------
  Lists
-------------------------------------------------------------------------------}

partitionOn :: forall a b. Ord b => (a -> b) -> [a] -> [(b, [a])]
partitionOn f = go Map.empty
  where
    go :: Map b [a] -> [a] -> [(b, [a])]
    go acc []     = Map.toList $ fmap reverse acc
    go acc (a:as) = go (Map.alter insert (f a) acc) as
      where
        insert :: Maybe [a] -> Maybe [a]
        insert = Just . maybe [] (a:)

explode :: forall a. Eq a => a -> [a] -> [[a]]
explode needle = go []
  where
    go :: [a] -> [a] -> [[a]]
    go acc []       = [reverse acc]
    go acc (x:xs)
      | x == needle = reverse acc : go [] xs
      | otherwise   = go (x:acc) xs

-- | Variation on 'explode' for 'Text'
explodeText :: Char -> Text -> [Text]
explodeText needle = map Text.pack . explode needle . Text.unpack

-- | Version of 'explode' where the needle is itself a list
explode' :: forall a. Eq a => [a] -> [a] -> [[a]]
explode' needle = go []
  where
    go :: [a] -> [a] -> [[a]]
    go acc [] = [reverse acc]
    go acc (x:xs)
      | Just xs' <- stripPrefix needle (x:xs) = reverse acc : go [] xs'
      | otherwise                             = go (x:acc) xs

trim :: String -> String
trim = ltrim . rtrim
  where
    ltrim, rtrim :: String -> String
    ltrim = dropWhile isSpace
    rtrim = reverse . dropWhile isSpace . reverse

indexInOrder :: [a -> Bool] -> [a] -> [a]
indexInOrder []     _  = []
indexInOrder (p:ps) xs = filter p xs ++ indexInOrder ps xs

{-------------------------------------------------------------------------------
  Tagsoup
-------------------------------------------------------------------------------}

findAttr :: String -> [Attribute String] -> Maybe String
findAttr attr = fmap snd . find isAttr
  where
    isAttr :: Attribute String -> Bool
    isAttr (attr', _) = attr == attr'

-- | Extract all matching sublists
--
-- Useful when parsing tagsoup
parseSoupWith :: ([a] -> Maybe (b, [a])) -> [a] -> [b]
parseSoupWith _ []     = []
parseSoupWith f (x:xs) =
    case f (x:xs) of
      Just (b, leftover) -> b : parseSoupWith f leftover
      Nothing            -> parseSoupWith f xs

{-------------------------------------------------------------------------------
  Servant
-------------------------------------------------------------------------------}

newtype HtmlPage (name :: Symbol) = HtmlPage { htmlPage :: Html }

instance ToMarkup (HtmlPage name) where
  toMarkup = htmlPage
