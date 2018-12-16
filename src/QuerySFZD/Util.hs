{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module QuerySFZD.Util (
    -- * Tagsoup
    parseSoupWith
    -- * Servant
  , DynPath(..)
  , dynPathToString
  ) where

import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Servant
import           Servant.Client
import           Servant.Client.Core.Internal.Request (appendToPath)

{-------------------------------------------------------------------------------
  Tagsoup
-------------------------------------------------------------------------------}

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
