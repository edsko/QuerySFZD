{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module QuerySFZD.API.Theirs.ShuFaZiDian (
    API
  , api
    -- * SFZD specific types
  , SfzdArgs(..)
  , module Export
  ) where

import Data.Proxy
import Servant
import Servant.HTML.Blaze
import Web.FormUrlEncoded

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import QuerySFZD.API.Ours.Query
import QuerySFZD.API.Theirs.ShuFaZiDian.Results as Export

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

type API = Search

type Search = ReqBody '[FormUrlEncoded] SfzdArgs
           :> Post '[HTML] SfzdResults

api :: Proxy API
api = Proxy

{-------------------------------------------------------------------------------
  SFZD specific types
-------------------------------------------------------------------------------}

data SfzdArgs = SfzdArgs {
      sfzdChar  :: SearchChar
    , sfzdStyle :: Style
    }

instance ToForm SfzdArgs where
  toForm SfzdArgs{..} = Form $ HashMap.fromList [
        ("wd",   [toQueryParam (SFZD sfzdChar)])
      , ("sort", [toQueryParam (SFZD sfzdStyle)])
      ]

{-------------------------------------------------------------------------------
  SFZD specific unparsers

  Since we package everything up into a form, these are used internally only.
-------------------------------------------------------------------------------}

newtype SFZD a = SFZD a

instance ToHttpApiData (SFZD SearchChar) where
  toQueryParam (SFZD (SearchChar c)) = Text.pack [c]

-- | Map our styles to SFZD
--
-- NOTE: We map 'small' to 钢笔 rather than 小楷
instance ToHttpApiData (SFZD Style) where
  toQueryParam (SFZD SemiCursive) = "8"
  toQueryParam (SFZD Regular)     = "9"
  toQueryParam (SFZD Cursive)     = "7"
  toQueryParam (SFZD Clerical)    = "6"
  toQueryParam (SFZD Seal)        = "3"
  toQueryParam (SFZD Small)       = "gangbi"
