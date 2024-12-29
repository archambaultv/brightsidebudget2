{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Calendar
    ( 
        dateAsDay,
        dayAsDate
    )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

dateAsDay :: Text -> Either Text Day
dateAsDay dt | T.null dt = error "empty date"
dateAsDay dt =
    case parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack dt) of
        Nothing -> Left $ "invalid date format " <> dt
        Just d -> Right d

dayAsDate :: Day -> Text
dayAsDate = T.pack . show