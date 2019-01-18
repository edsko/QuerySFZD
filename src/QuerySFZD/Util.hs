{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module QuerySFZD.Util (
    -- * Lists
    partitionOn
  , explode
  , explode'
  , trim
  , indexInOrder
    -- * Tagsoup
  , findAttr
  , parseSoupWith
    -- * Servant
  , DynPath(..)
  , dynPathToString
  ) where

import Data.Char (isSpace)
import Data.List (find, stripPrefix)
import Data.Map.Strict (Map)
import Data.String
import Data.Text (Text)
import Servant
import Servant.Client
import Servant.Client.Core.Internal.Request (appendToPath)
import Text.HTML.TagSoup (Attribute)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

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

  TODO: Do these already exist in servant-client somewhere? Should we submit
  a patc?
-------------------------------------------------------------------------------}

-- | Dynamic part of the path
newtype DynPath = DynPath Text
  deriving newtype IsString

dynPathToString :: DynPath -> String
dynPathToString (DynPath t) = Text.unpack t

instance HasClient m api => HasClient m (DynPath :> api) where
  type Client m (DynPath :> api) = DynPath -> Client m api

  clientWithRoute pm Proxy req (DynPath p) =
      clientWithRoute pm (Proxy @api) (appendToPath p req)

  hoistClientMonad pm _ f cl p =
      hoistClientMonad pm (Proxy @api) f (cl p)
