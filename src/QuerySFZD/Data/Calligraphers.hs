{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module QuerySFZD.Data.Calligraphers (
      Calligrapher(..)
    , knownCalligraphers
    , calligrapher
    , zhengkai
    , zhaoti
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
    , cAlts        :: [String]  -- ^ Alternative spellings, other names
    , cPinyin      :: String
    , cDates       :: String
    }
  deriving (Eq)

knownCalligraphers :: [Calligrapher]
knownCalligraphers = [
      Calligrapher {
          cSimplified  = "王羲之"
        , cTraditional = Nothing
        , cAlts        = []
        , cPinyin      = "Wáng Xīzhī"
        , cDates       = "303-361"
        }
    , Calligrapher {
          cSimplified  = "王献之"
        , cTraditional = Just "王獻之"
        , cAlts        = []
        , cPinyin      = "Wáng Xiànzhī"
        , cDates       = "344-386"
        }
    , Calligrapher {
          cSimplified  = "欧阳询"
        , cTraditional = Just "歐陽詢"
        , cAlts        = []
        , cPinyin      = "Ōuyáng Xún"
        , cDates       = "557-641"
        }
    , Calligrapher {
          cSimplified  = "颜真卿"
        , cTraditional = Just "顏真卿"
        , cAlts        = []
        , cPinyin      = "Yán Zhēnqīng"
        , cDates       = "709-785"
        }
    , Calligrapher {
          cSimplified  = "柳公权"
        , cTraditional = Just "柳公權"
        , cAlts        = []
        , cPinyin      = "Liǔ Gōngquán"
        , cDates       = "778-865"
        }
    , Calligrapher {
          cSimplified  = "米芾"
        , cTraditional = Nothing
        , cAlts        = ["米黻"]
        , cPinyin      = "Mǐ Fú"
        , cDates       = "1051–1107"
        }
    , Calligrapher {
          cSimplified  = "赵构"
        , cTraditional = Just "趙構"
        , cAlts        = ["宋高宗"]
        , cPinyin      = "Zhào Gòu"
        , cDates       = "1107-1187"
        }
    , Calligrapher {
          cSimplified  = "赵孟頫"
        , cTraditional = Just "趙孟頫"
        , cAlts        = []
        , cPinyin      = "Zhào Mèngfǔ"
        , cDates       = "1254–1322"
        }
    , Calligrapher {
          cSimplified  = "田英章"
        , cTraditional = Nothing
        , cAlts        = []
        , cPinyin      = "Tián Yīngzhāng"
        , cDates       = "1950-"
        }
    , Calligrapher {
          cSimplified  = "田蕴章"
        , cTraditional = Just "田蘊章"
        , cAlts        = []
        , cPinyin      = "Tián Yùnzhāng"
        , cDates       = "1945-"
        }
    , Calligrapher {
          cSimplified  = "翁闿运"
        , cTraditional = Just "翁闓運"
        , cAlts        = []
        , cPinyin      = "Wēng Kǎiyùn"
        , cDates       = "1912-2006"
        }
    ]

calligrapher :: String -> Calligrapher
calligrapher simplified =
    case filter ((== simplified) . cSimplified) knownCalligraphers of
      []         -> error $ "calligrapher: unknown calligrapher " ++ simplified
      [c]        -> c
      _otherwise -> error "calligrapher: more than one match"

zhengkai :: [Calligrapher]
zhengkai = [
      calligrapher "田英章" -- modern interpretation of 欧楷
    , calligrapher "田蕴章"
    , calligrapher "欧阳询"
    , calligrapher "颜真卿"
    , calligrapher "柳公权"
    , calligrapher "翁闿运" -- another modern calligrapher with a standard script
    ]

-- | Possible substitutes for 赵体
zhaoti :: [Calligrapher]
zhaoti = [
      calligrapher "赵孟頫"
    , calligrapher "王羲之"
    , calligrapher "赵构"
    ]

{-------------------------------------------------------------------------------
  Find/compare names
-------------------------------------------------------------------------------}

-- | Calligrapher name
--
-- This intentionally does not have an 'Eq' instance. See 'sameCalligrapher'.
newtype CalligrapherName = CalligrapherName { calligrapherNameToString :: String }
  deriving newtype (Show, Serialise, FromHttpApiData, ToHttpApiData)

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
        , n `elem` cAlts
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
