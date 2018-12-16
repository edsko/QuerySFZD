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
  , parseSoupWith
    -- * Servant
  , DynPath(..)
  , dynPathToString
  ) where

import Data.Map.Strict (Map)
import Data.String
import Data.Text (Text)
import Servant
import Servant.Client
import Servant.Client.Core.Internal.Request (appendToPath)

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
