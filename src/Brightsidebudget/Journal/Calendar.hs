{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Journal.Calendar
    ( 
        dateAsDay,
        dayAsDate,
        getYear,
        getMonth,
        getDay
    )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

dateAsDay :: Text -> Either Text Day
dateAsDay dt | T.null dt = error "empty date"
dateAsDay dt =
    case parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack dt) of
        Nothing -> Left $ "invalid date format " <> dt
        Just d -> Right d

dayAsDate :: Day -> Text
dayAsDate = T.pack . show

getYear :: Day -> Integer
getYear day = let (year, _, _) = toGregorian day in year

getMonth :: Day -> Int
getMonth day = let (_, month, _) = toGregorian day in month

getDay :: Day -> Int
getDay day = let (_, _, d) = toGregorian day in d