{-# LANGUAGE OverloadedStrings #-}

module Brightsidebudget.Calendar
    ( dateAsDay
    )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

dateAsDay :: Text -> Either Text Day
dateAsDay dt = 
    case parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack dt) of
        Nothing -> Left $ T.pack $ "invalid date format " ++ T.unpack dt
        Just d -> Right d