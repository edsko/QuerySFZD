module QuerySFZD.Data.Calligraphers (
      Calligrapher(..)
    , calligraphers
    , calligrapher
    , zhengkai
    ) where

data Calligrapher = Calligrapher {
      cSimplified  :: String
    , cTraditional :: Maybe String
    , cAlts        :: [String]
    , cPinyin      :: String
    , cDates       :: String
    }

calligraphers :: [Calligrapher]
calligraphers = [
      Calligrapher "王羲之" Nothing        []       "Wáng Xīzhī"     "303-361"
    , Calligrapher "歐陽詢" Nothing        []       "Ōuyáng Xún"     "557-641"
    , Calligrapher "颜真卿" (Just "顏真卿") []       "Yán Zhēnqīng"   "709-785"
    , Calligrapher "柳公权" (Just "柳公權") []       "Liǔ Gōngquán"   "778-865"
    , Calligrapher "米芾"   Nothing        ["米黻"] "Mǐ Fú"          "1051–1107"
    , Calligrapher "赵孟頫" (Just "趙孟頫") []       "Zhào Mèngfǔ"    "1254–1322"
    , Calligrapher "田英章" Nothing        []       "Tián Yīngzhāng" "1950-"
    , Calligrapher "田蕴章" (Just "田蘊章") []       "Tián Yùnzhāng"  "1945-"
    ]

calligrapher :: String -> Calligrapher
calligrapher simplified =
    head $ filter ((== simplified) . cSimplified) calligraphers

zhengkai :: [Calligrapher]
zhengkai = [
      calligrapher "歐陽詢"
    , calligrapher "田英章" -- modern interpretation of Outi
    , calligrapher "田蕴章"
    , calligrapher "颜真卿"
    , calligrapher "柳公权"
    ]
