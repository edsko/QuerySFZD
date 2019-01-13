{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module QuerySFZD.Data.Calligraphers (
      Calligrapher(..)
    , knownCalligraphers
    , calligrapher
    , zhengkai
      -- * Names
    , CalligrapherName(..)
    , findCalligrapher
    , sameCalligrapher
    , nubCalligrapherNames
    ) where

import Codec.Serialise
import Data.Function (on)
import Data.List (nub)
import Servant

data Calligrapher = Calligrapher {
      cSimplified  :: String
    , cTraditional :: Maybe String
    , cAltSpelling :: [String]
    , cPinyin      :: String
    , cDates       :: String
    }
  deriving (Eq)

knownCalligraphers :: [Calligrapher]
knownCalligraphers = [
      Calligrapher "王羲之" Nothing        []       "Wáng Xīzhī"     "303-361"
    , Calligrapher "欧阳询" (Just "歐陽詢") []       "Ōuyáng Xún"     "557-641"
    , Calligrapher "颜真卿" (Just "顏真卿") []       "Yán Zhēnqīng"   "709-785"
    , Calligrapher "柳公权" (Just "柳公權") []       "Liǔ Gōngquán"   "778-865"
    , Calligrapher "米芾"   Nothing        ["米黻"] "Mǐ Fú"          "1051–1107"
    , Calligrapher "赵孟頫" (Just "趙孟頫") []       "Zhào Mèngfǔ"    "1254–1322"
    , Calligrapher "田英章" Nothing        []       "Tián Yīngzhāng" "1950-"
    , Calligrapher "田蕴章" (Just "田蘊章") []       "Tián Yùnzhāng"  "1945-"
    ]

calligrapher :: String -> Calligrapher
calligrapher simplified =
    head $ filter ((== simplified) . cSimplified) knownCalligraphers

zhengkai :: [Calligrapher]
zhengkai = [
      calligrapher "歐陽詢"
    , calligrapher "田英章" -- modern interpretation of 欧楷
    , calligrapher "田蕴章"
    , calligrapher "颜真卿"
    , calligrapher "柳公权"
    ]

{-------------------------------------------------------------------------------
  Find/compare names
-------------------------------------------------------------------------------}

-- | Calligrapher name
--
-- This intentionally does not have an 'Eq' instance. See 'sameCalligrapher'.
newtype CalligrapherName = CalligrapherName { calligrapherNameToString :: String }
  deriving newtype (Show, Serialise, FromHttpApiData)

-- | Find information about a certain calligrapher
--
-- Returns the name if we have no information about this calligrapher.
findCalligrapher :: CalligrapherName -> Either String Calligrapher
findCalligrapher (CalligrapherName n) =
    case filter matches knownCalligraphers of
      []         -> Left n
      [c]        -> Right c
      _otherwise -> error "findCalligrapher: more than one match"
  where
    matches :: Calligrapher -> Bool
    matches Calligrapher{..} = or [
          cSimplified  == n
        , cTraditional == Just n
        , n `elem` cAltSpelling
        ]

sameCalligrapher :: CalligrapherName -> CalligrapherName -> Bool
sameCalligrapher = go `on` findCalligrapher
  where
    go :: Either String Calligrapher -> Either String Calligrapher -> Bool
    -- If we have no information about these calligraphers, then we fall back
    -- to comparing names
    go (Left n) (Left n')   = n == n'
    -- If we do have information about both calligraphers, suffices to compare
    -- the simplified name
    go (Right c) (Right c') = cSimplified c == cSimplified c'
    -- If we have information about one but not the other, can't be the same
    go _ _                  = False

nubCalligrapherNames :: [CalligrapherName] -> [CalligrapherName]
nubCalligrapherNames =
      map (either CalligrapherName (CalligrapherName . cSimplified))
    . nub
    . map findCalligrapher
